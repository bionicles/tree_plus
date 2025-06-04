from tree_plus_src import web


def test_articles_from_hacker_news():
    articles = web.articles_from_hacker_news(max_depth=1, n_articles=1)
    assert bool(articles)


def test_create_link_wiki():
    link = web.create_link("wiki", "Stack Overflow")
    assert "https://en.wikipedia.org/wiki/stack_overflow" in link
