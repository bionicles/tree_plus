# usage: python ~/hax/tree_plus/tree_plus_programs/rewrite.py -i gradients.h -o gradients.rs
from pathlib import Path
from typing import Optional, Tuple, List
from enum import Enum

from rich.console import Console
from rich.theme import Theme
from rich.traceback import install
from rich.panel import Panel
from rich import box

from transformers import AutoTokenizer, AutoModelForCausalLM, TextStreamer
import click
import torch

import tree_plus_src as tp

install(show_locals=True)

console = Console(
    style="#FFB000 on black",
    # this avoids a bug with '::' in code
    theme=Theme({"repr.ipv6": "default"}),
    width=128,
    soft_wrap=False,
)


class ModelName(Enum):
    HERMES = "TheBloke/OpenHermes-2.5-Mistral-7B-AWQ"
    MISTRAL = "mistralai/Mistral-7B-Instruct-v0.2"
    PHI = "microsoft/Phi-3-medium-128k-instruct"


DEFAULT_MODEL = ModelName.MISTRAL.value

tokenizer = AutoTokenizer.from_pretrained(DEFAULT_MODEL)
model = AutoModelForCausalLM.from_pretrained(
    DEFAULT_MODEL,
    # torch_dtype=torch.bfloat16,
    # device_map="auto",
    attn_implementation="flash_attention_2",
    device_map="cuda",
    trust_remote_code=True,
    torch_dtype="auto",
)

template = """{% for message in messages %}
{{ message['role']}}: {{ message['content'] }}
{% endfor %}
{{ eos_token }}"""


def rewrite_module(
    input_path: str,
    output_path: Optional[str],
    rewrite_ok: bool,
    suffix: Optional[str] = ".rs",
) -> Tuple[List[str], str, bool]:
    category = tp.engine.categorize(input_path)
    if category is not tp.engine.Category.FILE:
        raise ValueError("only files for now!")

    # verify the input
    input_path = Path(input_path)
    assert input_path.exists(), f"Nonexistent {input_path=}"

    if output_path:
        output_path = Path(output_path)
    elif suffix is not None:
        output_path = input_path.with_suffix(suffix)
        console.print(f"gonna put this test at {output_path=}")
    else:
        raise ValueError(
            "Unclear destination: Please clarify rewrite output path or suffix."
        )

    if input_path.suffix == output_path.suffix:
        raise ValueError(
            f"Useless conversion: cannot rewrite '{input_path.suffix}' in '{output_path.suffix}'"
        )

    if output_path.exists() and not rewrite_ok:
        raise ValueError(
            "not gonna clobber existing tests! try a name that doesn't exist"
        )

    # INPUT VERIFIED HERE!
    console.print(f"{input_path=}")
    console.print(f"{output_path=}")

    try:
        seed = str(input_path)
        input_tree = tp.from_seed(seed)
    except Exception as e:
        console.log(f"Exception parsing file: {e}")
        raise e

    if not input_tree:
        console.log(f"TreePlus.engine.from_seed failed on {input_path=}")

    input_tree.render()

    # build the chat history
    chat = [
        {
            "role": "system",
            "content": f"""SYSTEM(
Objective: translate '{input_path.suffix}' code into valid '{output_path.suffix}' code
Input: you will receive a high level API contract for some '{input_path.suffix}' code. 
Output: you will generate a low level API implementation for the same contract, but using '{output_path.suffix}' code.
Expectation: the code will be complete, test-driven, and will achieve the same function as the prior code, using the new language.

Note: remember not to get stuck in a loop. Be concise.
)""",
        },
        {
            "role": "user",
            "content": input_tree.into_str(),
        },
    ]
    processed_chat = tokenizer.apply_chat_template(
        chat,
        tokenize=False,
        add_generation_prompt=True,
        chat_template=template,
    )
    console.print(Panel(processed_chat, title="processed_chat", expand=True))

    input_ids = tokenizer(processed_chat, return_tensors="pt").to(model.device)
    streamer = TextStreamer(tokenizer)

    # Prepare to generate
    start_event = torch.cuda.Event(enable_timing=True)
    end_event = torch.cuda.Event(enable_timing=True)
    torch.cuda.reset_peak_memory_stats(model.device)
    torch.cuda.empty_cache()
    torch.cuda.synchronize()

    # Generate the code
    start_event.record()
    response = model.generate(
        **input_ids,
        do_sample=False,
        max_new_tokens=4096,
        streamer=streamer,
        # prompt_lookup_num_tokens=10,
    )

    # finish the timer and flush the gpu kernels
    end_event.record()
    torch.cuda.synchronize()

    # log some statistics
    max_memory = torch.cuda.max_memory_allocated(model.device)
    console.print("Max memory (MB): ", max_memory * 1e-6)
    new_tokens = response.shape[1] - input_ids.input_ids.shape[1]
    console.print(
        "Throughput (tokens/sec): ",
        new_tokens / (start_event.elapsed_time(end_event) * 1.0e-3),
    )

    convo = tokenizer.batch_decode(response, skip_special_tokens=True)[0]
    n = len(processed_chat)

    convo_input, convo_output = convo[:n], convo[n:]

    console.print(Panel(convo_input, title="Input", box=box.HORIZONTALS, expand=False))
    console.print(
        Panel(convo_output, title="Output", box=box.HORIZONTALS, expand=False)
    )

    assert 0

    # save the new code to the output path
    tp.deploy.load(content=convo_output, path=output_path)


this_file_path = Path(__file__)
default_output_path = this_file_path.with_suffix(".rs")


@click.command()
@click.option(
    "--input-path",
    "-i",
    help=f"path to the input file ({this_file_path})",
    default=this_file_path,
    type=Path,
)
@click.option(
    "--output-path",
    "-o",
    help=f"path to the output file ({default_output_path})",
    default=default_output_path,
    type=Path,
)
@click.option(
    "--rewrite-ok",
    "-r",
    help="should we overwrite the output file if it exists? y/n (default n)",
    is_flag=True,
    default=False,
)
def main(
    input_path: Path,
    output_path: Optional[Path],
    rewrite_ok: bool,
):
    "rewrite input_path as output_path"
    rewrite_module(
        input_path=input_path,
        output_path=output_path,
        rewrite_ok=rewrite_ok,
    )


if __name__ == "__main__":
    print(f"{__file__=}")
    main()
