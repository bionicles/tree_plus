from typing import Dict, Literal, Union
from urllib.parse import quote

from tree_plus_src import debug_print

Action = Union[
    Literal["search_stack_overflow"],
    Literal["search_wikipedia"],
    Literal["search_youtube"],
    Literal["search_google"],
    Literal["wiki"],
]

ACTION_NAMES = (
    "search_stack_overflow",
    "search_wikipedia",
    "search_youtube",
    "search_google",
    "wiki",
)

N_GOOGLE_RESULTS = 2  # small number of initial results to make room for recursion
LINK_STYLE = "#0C7BDC"  # web link blue color

# TODO: re-enable tree plus web actions


def create_url(kind: Action, query: str) -> str:
    "builds urls to GET"
    url = None
    if kind == "search_wikipedia":
        url = create_wikipedia_search_url(query)
    elif kind == "wiki":
        url = create_wikipedia_url(query)
    elif kind == "search_google":
        url = create_google_search_url(query)
    assert url is not None
    return url


def create_link(kind: Action, query: str) -> str:
    "builds links to click"
    link = None
    if kind == "search_wikipedia":
        link = create_wikipedia_search_link(query)
    elif kind == "wiki":
        link = create_google_search_link(query)
    elif kind == "search_google":
        link = create_stack_overflow_search_link(query)
    assert link is not None
    return link


def create_wikipedia_url(subterm: str) -> str:
    prepared_subterm = subterm.lower().replace(" ", "_")
    wiki_url = f"https://en.wikipedia.org/wiki/{prepared_subterm}"
    return wiki_url


def create_wikipedia_search_url(subterm: str) -> str:
    quoted_subterm = quote(subterm)
    wiki_search_url = f"https://en.wikipedia.org/w/index.php?title=Special:Search"
    wiki_search_url += f"&search={quoted_subterm}"
    return wiki_search_url


def create_google_search_url(subterm: str) -> str:
    quoted_subterm = quote(subterm)
    google_search_url = f"https://www.google.com/search?q={quoted_subterm}"
    return google_search_url


def create_stack_overflow_search_url(subterm: str) -> str:
    quoted_subterm = quote(subterm)
    stack_overflow_search_url = f"https://stackoverflow.com/search?q={quoted_subterm}"
    return stack_overflow_search_url


def create_wikipedia_search_link(
    subterm: str,
    prefix: str = "",
    suffix: str = "",
    link_style: str = LINK_STYLE,
) -> str:
    wikipedia_search_url = create_wikipedia_search_url(subterm)
    wikipedia_search_link = f"[{link_style}][link={wikipedia_search_url}]"
    wikipedia_search_link += f"{prefix}{subterm}{suffix}"
    wikipedia_search_link += f"[/link][/{link_style}]"
    return wikipedia_search_link


def create_google_search_link(
    subterm: str,
    prefix: str = "",
    suffix: str = "",
    link_style: str = LINK_STYLE,
) -> str:
    google_search_url = create_google_search_url(subterm)
    google_search_link = f"[{link_style}][link={google_search_url}]"
    google_search_link += f"{prefix}{subterm}{suffix}"
    google_search_link += f"[/link][/{link_style}]"
    return google_search_link


def create_stack_overflow_search_link(
    subterm: str,
    prefix: str = "",
    suffix: str = "",
    link_style: str = LINK_STYLE,
) -> str:
    stack_overflow_search_url = create_stack_overflow_search_url(subterm)
    stack_overflow_search_link = f"[{link_style}][link={stack_overflow_search_url}]"
    stack_overflow_search_link += f"{prefix}{subterm}{suffix}"
    stack_overflow_search_link += f"[/link][/{link_style}]"
    return stack_overflow_search_link


from typing import Tuple, Optional, Mapping

# from googlesearch import search, SearchResult


# def search_google(
#     query: str, n_rows: int = N_GOOGLE_RESULTS
# ) -> Tuple[SearchResult, ...]:
#     results = tuple(search(query, advanced=True, safe="", num_results=n_rows))
#     # df = pl.DataFrame(
#     #     [
#     #         dict(title=result.title, url=result.url, description=result.description)
#     #         for result in results
#     #     ]
#     # )
#     return results


from rich.table import Table


def search_stack_overflow(
    *,
    titled: Optional[str] = None,
    tagged: Optional[str] = None,
    answered: Optional[bool] = True,
) -> Tuple[Tuple[dict, Tuple[dict, ...]], ...]:
    from pystackapi import Site
    from pystackapi.sites import StackOverflow

    if titled is None and tagged is None:
        raise ValueError(
            "tree_plus_src.web.seach_stack_exchange needs 'titled' or 'tagged'"
        )

    site = Site(StackOverflow)

    # Search for questions with word "Python" in title
    results = site.search(intitle=titled, tagged="Python")
    if answered is not None:
        results = tuple(filter(lambda r: r.is_answered == answered, results))

    debug_print(results)

    questions_with_answers = []
    for question in results:
        time.sleep(0.02)  # oops, forgot this, got blocked!
        print(question, type(question))
        question = dict(question)
        # NOTE: no point in the answers since there's no content
        # answer_count = question["answer_count"]
        # answers = ()
        # if answer_count:
        #     print("GET ANSWERS!")
        #     try:
        #         answers = site.get_answers_on_questions([question["question_id"]])
        #         # print("ANSWERS:", answers)
        #         # full_answers = []
        #         # for a in answers:
        #         #     try:
        #         #         # a_2 = site.get_answer(a["answer_id"])
        #         #         print("ANSWER_1:", a)
        #         #         print("ANSWER_2:", a_2)
        #         #         if a_2 is None:
        #         #             continue
        #         #         full_answers.append(dict(a_2))
        #         #     except Exception as e:
        #         #         raise e
        #         answers = tuple(dict(a) for a in answers)
        #     except Exception as e:
        #         raise e
        unit = (question, ())
        debug_print("unit", unit)
        questions_with_answers.append(unit)

    questions_with_answers = tuple(questions_with_answers)
    return questions_with_answers


# def format_stack_overflow_result(result: dict) -> dict:
#     # from datetime import datetime

#     formatted_result = {
#         # "creation_date": str(
#         #     datetime.fromtimestamp(result["creation_date"]).isoformat()
#         # ),
#         # "last_activity_date": str(
#         #     datetime.fromtimestamp(result["last_activity_date"]).isoformat()
#         # ),
#         "question_id": f'[link=https://stackoverflow.com/questions/{result["question_id"]}]{result["question_id"]}[/link]',
#         "tags": "- " + "\n- ".join(result["tags"]),
#         # "owner_display_name": result["owner"]["display_name"],
#         "title": result["title"],
#         "score": result["score"],
#         "answer_count": result["answer_count"],
#     }

#     return formatted_result


def table_from_df(
    results: Tuple[Dict[str, str], ...],
    title: Optional[str] = None,
    show_lines: bool = True,
) -> Table:
    if not results:
        raise ValueError("no results")
    columns = tuple(results[0].keys())
    table = Table(*columns, title=title, show_lines=show_lines)
    for result in results:
        row = tuple(result.values())
        table.add_row(*map(str, row))
    return table


from typing import List
import requests, time

Articles = Tuple[Tuple[dict, Tuple[dict, ...]], ...]
HList = Tuple[dict, "HList"]

from rich.progress import track


def articles_from_hacker_news(
    max_depth: int = 2,  # Use -1 for unlimited depth
    current_depth: int = 0,
    n_articles: int = 3,
    sleep_time: float = 0.00,
) -> Articles:
    "a tuple of tuples looking like ((dict, (dict, ...)), ...)"
    ids = requests.get("https://hacker-news.firebaseio.com/v0/topstories.json")
    ids_json = ids.json()
    debug_print("ids:", ids_json)
    articles = []
    for article_id in ids_json:
        if current_depth > max_depth >= 0:
            break
        (article, comments) = hacker_news_article_from_id(
            article_id,
            max_depth,
            current_depth + 1,
            n_articles,
            sleep_time,
        )
        if article:
            articles.append((article, comments))
        if len(articles) == n_articles:
            break
    return tuple(articles)


def hacker_news_article_from_id(
    article_id: int,
    depth: int,
    current_depth: int,
    n: int,
    sleep_time: float,
) -> Tuple[HList, Tuple[HList, ...]]:
    article_response = requests.get(
        f"https://hacker-news.firebaseio.com/v0/item/{article_id}.json"
    )
    article_json = article_response.json()
    debug_print("article_json:", article_json)
    kids: Tuple[HList, ...] = ()
    if "kids" in article_json:
        kids = fetch_kids(
            article_id,
            article_json["kids"],
            depth,
            current_depth,
            n,
            sleep_time,
        )
    return (article_json, kids)


def fetch_kids(
    article_id: int,
    kid_ids: List[int],
    depth: int,
    current_depth: int,
    n: int,
    sleep_time: float,
) -> Tuple[HList, ...]:
    kids = []
    for kid_id in kid_ids:
        if current_depth > depth >= 0:
            break
        (kid, comments) = hacker_news_article_from_id(
            kid_id,
            depth,
            current_depth + 1,
            n,
            sleep_time,
        )
        kids.append(
            (kid, comments)
        )  # Initialize with empty list, will be filled recursively
        if len(kids) == n:
            break
        time.sleep(sleep_time)
    kids = tuple(kids)
    return kids
