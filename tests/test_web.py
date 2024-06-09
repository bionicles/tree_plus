from tree_plus_src import web


def test_articles_from_hacker_news():
    articles = web.articles_from_hacker_news()
    assert bool(articles)
