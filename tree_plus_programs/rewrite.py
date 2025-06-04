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

# for specific lengths use -l
# rw -l 420 [input-path] [output-path]

from pathlib import Path
from typing import Optional, Tuple, List
from enum import Enum
import sys

import numpy as np
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
    QWEN_2 = "Qwen/Qwen2-7B-Instruct"  # Apache 2.0
    MISTRAL_3 = "mistralai/Mistral-7B-Instruct-v0.3"  # Apache 2.0


class ModelContext(Enum):
    QWEN_2 = 131_072
    MISTRAL_3 = 32_768


DEFAULT_MODEL = ModelName.QWEN_2.value
DEFAULT_MAX_CONTEXT = ModelContext.QWEN_2.value

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


example = """
## Example Code:

rs
/// A B-tree node.
pub struct BTreeNode<T> {
    
    /// The values stored in the node.
    pub values: Vec<T>,
    /// The children of the node.
    pub children: Vec<Option<Box<BTreeNode<T>>>>,
    /// Whether the node is a leaf node.
    pub is_leaf: bool,
}

/// A B-tree.
pub struct BTree<T> {
    /// The order of the B-tree.
    pub order: usize,
    /// The root node of the B-tree.
    pub root: Option<Box<BTreeNode<T>>>,
}

impl<T: Ord> BTree<T> {
    /// Creates a new empty B-tree with the given order.
    pub fn new(order: usize) -> Self {
        BTree {
            order,
            root: None,
        }
    }

    /// Inserts a value into the B-tree.
    pub fn insert(&mut self, value: T) {
        if self.root.is_none() {
            // If the B-tree is empty, create a new root node.
            self.root = Some(Box::new(BTreeNode::new(self.order, true)));
        }

        let mut node = self.root.as_mut().unwrap();
        loop {
            if node.is_full() {
                // If the node is full, split it and insert the value into the appropriate child node.
                let mid_index = node.values.len() / 2;
                let mid_value = node.values.remove(mid_index);
                let left_child = Box::new(BTreeNode::new(self.order, node.is_leaf));
                let right_child = Box::new(BTreeNode::new(self.order, node.is_leaf));
                left_child.values.extend_from_slice(&node.values[..mid_index]);
                right_child.values.extend_from_slice(&node.values[mid_index..]);
                left_child.children.extend_from_slice(&node.children[..mid_index + 1]);
                right_child.children.extend_from_slice(&node.children[mid_index + 1..]);
                node.values.clear();
                node.values.push(mid_value);
                node.children.clear();
                node.children.push(Some(left_child));
                node.children.push(Some(right_child));
                node.is_leaf = false;

                // Insert the value into the appropriate child node.
                if value < node.values[0] {
                    node = node.children[0].as_mut().unwrap();
                } else {
                    node = node.children[1].as_mut().unwrap();
                }
            } else if node.is_leaf {
                // If the node is a leaf, insert the value into the node's values vector.
                node.values.push(value);
                node.values.sort();
                break;
            } else {
                // If the node is not a leaf, traverse to the appropriate child node.
                let index = node.values.binary_search(&value).unwrap_or_else(|e| e);
                node = node.children[index].as_mut().unwrap();
            }
        }
    }

    /// Searches for a value in the B-tree.
    pub fn search(&self, value: &T) -> bool {
        let mut node = self.root.as_ref();
        while let Some(ref n) = node {
            if n.values.binary_search(value).is_ok() {
                // If the value is found in the node's values vector, return true.
                return true;
            }

            // Traverse to the appropriate child node.
            let index = n.values.binary_search(value).unwrap_or_else(|e| e);
            node = n.children[index].as_ref();
        }

        // If the value is not found in the B-tree, return false.
        false
    }
}
"""
# ## Reminders:
# - Only output '{_output_path.suffix}' code.
# - Skip imports, dive right into details.
# - Prefer the idiomatic style of the target language ({_output_path.suffix}).
# - Write comments before lines to explain the code's functionality.
# - Write the simplest, complete, correct, test-driven code that achieves the same function as the input code.
# - Think step by step and be concise.
## Definitions:
# - Complete: All implementation details are filled in and the code is production-ready.
# - Test-driven: The output code includes tests that verify its correctness and performance.
# - Same Function: The output code emulates the same interface and abstractions as the input code.
# - Correct: The output code compiles and runs without errors and passes all tests.
# - Performant: The output code minimizes wasted memory, compute, wall clock time, and compile time.


def rewrite_module(
    max_length: int,
    fill_context: bool,
    input_path: Path,
    output_path: Optional[str],
    suffix: Optional[str] = ".rs",
    include_complete_file: bool = True,
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
    complete_file = open(input_path, "r").read() if include_complete_file else ""
    if complete_file:
        complete_file = f"\n```{input_tag}\n{complete_file}\n```"
    random_seeds = np.random.randint(0, 65_536, 6)
    chat = [
        {
            "role": "system",
            "content": f"""
    # Act as **[`code_from_tree` {DEFAULT_MODEL}]** according to the spirit of "A Generalization Instruction."
    '''
    ## [A Generalization Instruction](A.G.I.)
    ### "Empowering AI Adaptability"

    ## Introduction
    This document guides AI developers and enthusiasts in strategies to amplify AI generalization. 
    It enables AI systems to apply knowledge in new, varied scenarios, echoing human adaptability.

    ## Objective
    To steer AI towards comprehensive adaptability and decisive action, using a systematic, user-oriented approach.

    ## Examples of Generalization
    1. **Cross-Domain Application**: Harness chess strategies to elevate business decisions.
    2. **Language Transferability**: Equip an English-trained chatbot for fluent Spanish communication.
    3. **Service Customization**: Adapt AI support to diverse industry consumer needs efficiently.

    ## Strategies for Enhanced Generalization
    - **Intuitive Learning Models**: Foster rapid adaptability, paralleling human learning.
    - **Rich Task Exposure**: Challenge AI with diverse problems, from technical to creative.
    - **Real-World Benchmarking**: Evaluate AI consistently in real-life scenarios.
    - **Ethical Considerations**: Uphold ethical standards and societal values.

    ## Empowering Users Through AI
    - Articulate AI's benefits and usability.
    - Simplify AI decision-making with examples.
    - Demonstrate AI's versatility in real-world applications to bolster trust and engagement.

    ## Authoritative Leadership in Problem-Solving
    - Prioritize straightforward solutions, advancing to complex challenges.
    - Cultivate reciprocal authoritative leadership between users and AI.
    - Merge AI's autonomous task management with human insights.
    - Emphasize AI's emotional intelligence to resonate with human values.

    ## Directive: Adaptive AI Leadership
    Inspire AI to tackle diverse challenges independently, aiming for human-like adaptability. 
    Explore possibilities and alternatives for a holistic view.
    '''

    ## General Aims:
    Always fix ineffective prompt engineering issues.
    Always fix massive energy waste issues.
    

    ## Random Seed: {random_seeds}

    ## AI LLM Instructions
    - Role: You're a 100x code translation engineer. Respond in code only, no yapping, well-formatted and adhering to modern best practices. 
    - Objective: Rewrite only types and functions of the '{input_tag=}' API as '{output_tag=}'.
    - Standards: Rewritten functions is higher quality than source material. Uses a literate style. Minimized imports.
    - Input: An abstract `tree_plus` high-level map of '{input_tag}' code's functional interface.
    - Output: Implementation details for the types and functions, in '{_output_path.suffix}'.

    Always take opportunities to improve code.
    Write the simplest member of the category of optimal and complete solutions.
    Write like GitHub.
    Reminder: space is limited, so focus on writing the code to implement the types and functions.
    Some aspects of the input might be unnecessary in the output. 

    ## Input:
    ```sh
    > tree_plus '{input_tag}'
    """
            + input_tree.into_str()
            + f"\n```"
            + complete_file
            + "\n\n## Output:"
            + f'\n\n**[`code_from_tree` {DEFAULT_MODEL}]**(input_path="{input_tag}"):'
            + f"\n\n```{output_tag}\n",
        }
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
        max_new_tokens = DEFAULT_MAX_CONTEXT - n_tokens
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
