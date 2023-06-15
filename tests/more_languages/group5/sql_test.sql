CREATE TABLE promoters (
    user_id serial PRIMARY KEY,
    type varchar(20) NOT NULL,
    username varchar(20) NOT NULL,
    password varchar(20) NOT NULL,
    email varchar(30) NOT NULL,
    phone varchar(20) NOT NULL,
    promocode varchar(20),
    info json,
    going text[],
    invites text[],
    balance integer NOT NULL,
    rewards text[],
    created timestamp
);

CREATE TABLE events (
    event_id serial PRIMARY KEY,
    name varchar(64) NOT NULL,
    date varchar(64) NOT NULL,
    location varchar(64) NOT NULL,
    performer varchar(64) NOT NULL,
    rewards json,
    created timestamp
);



info json promoter 'a':
{
    "events_invites": {
        "Joel1": {
            "b": "yes",
            "c": "no",
            "d": "yes"
        }, 
        "Music Diner's6": {
            "c": "yes"
        }
    },
    "events_going": {
        "Joel1" : "yes"
    }
}

pricesAndRewards json event_1: 
{
    "0 sales": ["$80"],
    "2 sales": ["$65"],
    "5 sales": ["$50", "Skip Line", "2 Drinks"],
    "15 sales": ["$0", "Skip Line", "5 Drinks", "5% Commission"]
}