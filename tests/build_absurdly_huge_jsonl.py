import pathlib
import json


import polars as pl

N_REPEATS = 42_000


def generate_huge_jsonl(
    df: pl.DataFrame,
    output_file: pathlib.Path,
    repeat: int,
) -> bool:
    """
    Generate an absurdly huge JSONL file from a Polars DataFrame.

    Parameters:
    df (polars.DataFrame): The input DataFrame.
    output_file (str): The path to the output JSONL file.
    repeat (int): The number of times to repeat the DataFrame in the output file.
    """
    with open(output_file, "w+") as f:
        for _ in range(repeat):
            for row in df.iter_rows():
                # make sure we keep the keys of the json ... dumping just the row only gives values
                row_dict = {col: val for col, val in zip(df.columns, row)}
                f.write(json.dumps(row_dict) + "\n")

    return True


def main():
    df = pl.DataFrame(
        {
            "SMILES": ["CCO", "CC(=O)O", "CC(=O)OC1=CC=CC=C1"],
            "Yield": [0.85, 0.72, 0.63],
            "Temperature": [25, 30, 35],
            "Pressure": [1.0, 1.5, 2.0],
            "Solvent": ["Ethanol", "Water", "Methanol"],
            "Success": [True, False, True],
            "Reaction_Conditions": [
                {"Temperature": 25, "Pressure": 1.0, "Solvent": "Ethanol"},
                {"Temperature": 30, "Pressure": 1.5, "Solvent": "Water"},
                {"Temperature": 35, "Pressure": 2.0, "Solvent": "Methanol"},
            ],
            "Products": [
                ["Ethane", "Carbon Dioxide"],
                ["Ethanol", "Carbon Dioxide"],
                ["Methanol", "Carbon Dioxide"],
            ],
            "EdgeCasesMissed": [None, None, None],
        }
    )

    huge_jsonl_path = (
        pathlib.Path() / "tests" / "more_languages" / "group7" / "absurdly_huge.jsonl"
    )
    # Generate a huge JSONL file by repeating the DataFrame 100_000 times
    assert generate_huge_jsonl(df, huge_jsonl_path, N_REPEATS)
    print(f"Ok(huge_jsonl_path = {huge_jsonl_path})")


if __name__ == "__main__":
    main()
