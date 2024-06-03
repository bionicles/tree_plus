# Quickstart:
# 1. edit ~/.bash_profile or ~/.bashrc to add:
# alias rw="python ~/hax/tree_plus/tree_plus_programs/rewrite.py"
# 2. source your ~/.bash_profile or ~/.bashrc
# 3. invoke rewrite.py with arguments

# Usage:
# for help:
# rw -h
# rewrite input-path as output-path
# rw [input-path] [output-path]
# to max out context length use -C

# rewrite the tree_plus engine.py module in Rust:
# rw -C ../tree_plus_src/engine.py ./engine.rs

# for specific lenths use -l
# rw -l 420 [input-path] [output-path]

from pathlib import Path
from typing import Optional, Tuple, List
from enum import Enum
import sys

from rich.console import Console
from rich.theme import Theme
from rich.traceback import install
from rich.panel import Panel
from rich import box

from transformers import (
    AutoTokenizer,
    AutoModelForCausalLM,
    TextStreamer,
    BitsAndBytesConfig,
)
import click
import torch

import tree_plus_src as tp

DEFAULT_MAX_LENGTH = 256  # short default for fast demos
MAX_CONTEXT = 32_768

APPLE = sys.platform == "darwin"
NVIDIA = not APPLE


install(show_locals=True)

console = Console(
    style="#FFB000 on black",
    # this avoids a bug with '::' in code
    theme=Theme({"repr.ipv6": "default"}),
    width=128,
    soft_wrap=False,
    markup=False,
)


class ModelName(Enum):
    MISTRAL_3 = "mistralai/Mistral-7B-Instruct-v0.3"
    MISTRAL_2 = "mistralai/Mistral-7B-Instruct-v0.2"
    # HERMES = "TheBloke/OpenHermes-2.5-Mistral-7B-AWQ" # disabled for 32k uncertainty
    # PHI = "microsoft/Phi-3-medium-128k-instruct"


DEFAULT_MODEL = ModelName.MISTRAL_3.value

tokenizer = AutoTokenizer.from_pretrained(
    DEFAULT_MODEL,
    padding_side="left",
)
quantization_config = BitsAndBytesConfig(
    load_in_4bit=True,
    bnb_4bit_compute_dtype=torch.float16,
)
model = AutoModelForCausalLM.from_pretrained(
    DEFAULT_MODEL,
    # device_map="auto",
    torch_dtype="auto",
    attn_implementation="flash_attention_2",
    device_map="auto",
    trust_remote_code=True,
)

template = """{% for message in messages %}
{{ message['role']}}: {{ message['content'] }}
{% endfor %}
{{ eos_token }}"""


def rewrite_module(
    max_length: int,
    fill_context: bool,
    input_path: Path,
    output_path: Optional[str],
    suffix: Optional[str] = ".rs",
):
    category = tp.engine.categorize(input_path)
    if category is not tp.engine.Category.FILE:
        raise ValueError("only files for now!")

    # verify the input
    input_path = Path(input_path)
    assert input_path.exists(), f"Nonexistent {input_path=}"

    _output_path = None
    if output_path:
        _output_path = Path(output_path)
    elif suffix is not None:
        _output_path = input_path.with_suffix(suffix)
        console.print(f"gonna put this test at {output_path=}")
    else:
        raise ValueError(
            "Unclear destination: Please clarify rewrite output path or suffix."
        )
    assert _output_path is not None

    if input_path.suffix == _output_path.suffix:
        raise ValueError(
            f"Useless conversion: cannot rewrite '{input_path.suffix}' in '{_output_path.suffix}'"
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
    _output_path = _output_path.resolve()

    input_tag = f"{input_path.parent.parent.name}/{input_path.parent.name}/{str(input_path.name)}"
    output_tag = f"{_output_path.parent.parent.name}/{_output_path.parent.name}/{str(_output_path.name)}"
    chat = [
        {
            "role": "system",
            "content": f"""
# Act as **[`code_from_tree` Mistral]**

## AI LLM Instructions
- Role: you are a '{_output_path.suffix}' code generator who only outputs valid '{_output_path.suffix}' code / format files.
- Objective: translate '{input_path.suffix}' code into valid '{_output_path.suffix}' code / format files.
- Input: you will receive a high level API TreePlus contract for some '{input_path.suffix}' code / format files. 
- Output: you will generate a low level API implementation of the same contract, but using '{_output_path.suffix}' code / format files.
- Expectation: the output will be complete, test-driven, and will achieve the same function as the prior code, using the desired language.

## Definitions:
- Complete: means no placeholders or TODO remaining
- Test-driven: if you don't have a failing test case, then you're not doing TDD
- Same Function: emulate the same interface and abstractions correctly
- Correct: code compiles and runs and passes tests, both implicit and explicit, performantly
- Performantly: minimizing wasted memory, compute, and wall clock time

Remember: ONLY OUTPUT '{_output_path.suffix}' TO AVOID BREAKING LANGUAGE COMPILERS AND / OR INTERPRETERS. 

Make sure to fill in the implementation details. 
If the new language has an idiomatic style, prefer that.

DO NOT GET STUCK IN A LOOP. Be concise!

Write the simplest category member of complete, correct, test-driven, same function files.
)""",
        },
        {
            "role": "user",
            "content": f"\n> tree_plus '{input_path}'\n"
            + input_tree.into_str()
            + f'\n\n**[`code_from_tree` Mistral]**(input_path="{input_tag}"):'
            + f"\n```{output_tag}",
        },
    ]
    processed_chat = tokenizer.apply_chat_template(
        chat,
        tokenize=False,
        add_generation_prompt=True,
        chat_template=template,
    )
    console.print(Panel(processed_chat, title="processed_chat", box=box.HORIZONTALS))  # type: ignore

    input_ids = tokenizer(processed_chat, return_tensors="pt").to(model.device)  # type: ignore
    streamer = TextStreamer(tokenizer, skip_prompt=True)  # type: ignore

    # Prepare to generate
    # Generate the code
    if fill_context:
        n_tokens = len(input_ids.tokens())
        max_new_tokens = MAX_CONTEXT - n_tokens
    else:
        max_new_tokens = max_length
    console.rule("Begin Generation")
    # for some reason, I have tensorflow and torch happening, looks like this is torch, actually, whoops!
    start_event = None
    end_event = None
    if NVIDIA:
        start_event = torch.cuda.Event(enable_timing=True)
        end_event = torch.cuda.Event(enable_timing=True)
        torch.cuda.reset_peak_memory_stats(model.device)
        torch.cuda.empty_cache()
        torch.cuda.synchronize()
        start_event.record()  # type: ignore
    response = model.generate(
        **input_ids,
        do_sample=False,
        max_new_tokens=max_new_tokens,
        streamer=streamer,
        # prompt_lookup_num_tokens=10, # TODO: research various kwargs for huggingface / torch performance
    )

    # finish the timer and flush the gpu kernels
    console.rule("End Generation")
    if NVIDIA:
        end_event.record()  # type: ignore
        torch.cuda.synchronize()

        # log some statistics
        max_memory = torch.cuda.max_memory_allocated(model.device)
        console.print("Max memory (MB): ", max_memory * 1e-6)
        new_tokens = response.shape[1] - input_ids.input_ids.shape[1]
        if start_event is not None and end_event is not None:
            console.print(
                "Throughput (tokens/sec): ",
                new_tokens / (start_event.elapsed_time(end_event) * 1.0e-3),
            )

    # I think the conversation length calculation may be off due to the skipping of special tokens
    # Might need to remove these later for accuracy !
    convo = tokenizer.batch_decode(response, skip_special_tokens=False)[0]
    n = len(processed_chat)

    convo_input, convo_output = convo[:n], convo[n:]

    console.print(Panel(convo_input, title="Input", box=box.HORIZONTALS, expand=True))
    console.print(Panel(convo_output, title="Output", box=box.HORIZONTALS, expand=True))

    # assert 0 # oops

    # save the new code to the output path
    tp.deploy.load(content=convo_output, path=str(_output_path))


this_file_path = Path(__file__)
default_output_path = this_file_path.with_suffix(".rs")


@click.command()
@click.option(
    "--fill-context",
    "-C",
    help=f"fill the context window of the LLM (False)",
    default=False,
    is_flag=True,
)
@click.option(
    "--length",
    "-l",
    help=f"maximum output length ({DEFAULT_MAX_LENGTH})",
    default=DEFAULT_MAX_LENGTH,
    type=int,
)
@click.argument(
    "input-path",
    # help=f"path to the input file ({this_file_path})",
    default=this_file_path,
    type=click.Path(exists=True),
)
@click.argument(
    "output-path",
    # help=f"path to the output file ({default_output_path})",
    default=default_output_path,
    type=click.Path(exists=False),
)
def main(
    fill_context: bool,
    length: int,
    input_path: Path,
    output_path: Optional[click.Path],
    # rewrite_ok: bool,
):
    "rewrite input_path as output_path"
    length_or_context = None
    if fill_context:
        length_or_context = f"-C"
    else:
        length_or_context = f"-l {length}"
    command_panel = Panel(
        f"python rewrite.py {length_or_context} {input_path} {output_path}",
        expand=False,
        title="rewrite command",
    )
    console.print(command_panel)
    rewrite_module(
        fill_context=fill_context,
        max_length=length,
        input_path=input_path,
        output_path=str(output_path),
    )
    console.print(command_panel)


if __name__ == "__main__":
    print(f"{__file__=}")
    main()
