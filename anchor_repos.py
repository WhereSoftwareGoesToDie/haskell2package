"""Fetch a full list of the organisation's public repos, dump to a list."""

import sys
import os
import requests
from pprint import pprint as PP

ORG_REPOS_URL = 'https://api.github.com/orgs/anchor/repos'
OUTPUT_FILE = 'repos.list'

# We always assume that a link starts with  <URL_HERE>
SAMPLE_LINK_HEADER = '''<https://api.github.com/organizations/146304/repos?page=2>; rel="next", <https://api.github.com/organizations/146304/repos?page=4>; rel="last"'''


def parse_github_http_link_header(header):
    links = {}
    link_chunks = header.split(', ')
    for link in link_chunks:
        URL,rest = link.split('; ', 1)
        URL = URL.strip('<>')
        # Overkill, aeh.
        kv = dict([ tuple([y.strip('"') for y in x.split('=')]) for x in rest.split('; ') ])
        links[kv['rel']] = URL
    return links

def get_repos_from_org_listing(j):
    return [ x['name'] for x in j ]

def write_repos_to_output_file(repos):
    with open(OUTPUT_FILE, 'w') as f:
        for repo in repos:
            f.write("{}\n".format(repo))

def get_github_creds_from_env():
    gh_user = os.environ.get('GH_USER')
    gh_pass = os.environ.get('GH_PASS')
    return (gh_user,gh_pass)


def main(argv=None):
    org_repos = []
    gh_credentials = get_github_creds_from_env()

    next_url = ORG_REPOS_URL
    while next_url:
        print("Fetching {}".format(next_url))

        # This get() can throw an exception if you hit the API request limit.
        # If that occurs, you'll get no listing, potentially after doing a
        # bunch of work. C'est la vie.
        r = requests.get(next_url, auth=gh_credentials)

        org_repos += get_repos_from_org_listing( r.json() )
        link_header = r.headers['link']
        links = parse_github_http_link_header(link_header)
        next_url = links.get('next')

    org_repos.sort()
    write_repos_to_output_file(org_repos)
    return 0


if __name__ == "__main__":
    sys.exit(main())
