# haskellwebscrapper

Haskell library to recursively scrape a website and search for information

> Using this tool can potentially send a lot of requests to a website and should be used with care and respect. 
> a delay before each request is configured and customizable.

This tool is as much a self exercise in Haskell programming as it is a real, useful tool, that was useful in several occasions where a website did not provide me with an easy way to gather the information that was relevant to me.

It can authenticate to a website if needed, and will crawl it page by page based on what should be extracted at each step, and generate a result (downloading content or generated in any other way).

The natural flow adapted to this tool is when each page provides several links (a Tree structure), and at each leaf of the tree there is an action you would like to perform.

The tool tries to be resilient to network faults, and downloads will be retried if they fail, and previously downloaded results will be checked so that it can be run several times without generating everything from scratch.

