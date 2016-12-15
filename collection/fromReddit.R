# install the package "RedditExtractoR" first, if necessary
library("RedditExtractoR")

# define your query (search term) and subreddit
query = "bad luck"
subreddit = "memes"

# function to access the Reddit API, detailed settings can be edited below
GetPosts <- function(query, subreddit){
        reddit_urls(search_terms = query, 
                    subreddit = subreddit,
                    cn_threshold = 0, 
                    page_threshold = 300, 
                    sort_by = "comments",
                    wait_time = 2)
}

# run function and extract Reddit posts (may take a while)
posts <- GetPosts(query, subreddit)
