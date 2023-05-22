import time

from src import tree_plus


def test_performance_large_directory():
    start_time = time.time()
    result = tree_plus("/path/to/large/directory")
    end_time = time.time()
    # for instance, our function should take less than 5 seconds for a large directory
    assert end_time - start_time < 5
