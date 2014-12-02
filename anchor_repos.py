# pylint: disable=line-too-long
# pylint: disable=bad-whitespace
# pylint: disable=invalid-name
# pylint: disable=superfluous-parens
# pylint: disable=missing-docstring

"""Fetch a full list of the organisation's public repos, dump to a list."""

import sys
import os
import requests

ORG_REPOS_URL = 'https://api.github.com/orgs/anchor/repos'
OUTPUT_FILE = 'repos.list'


def parse_github_http_link_header(header):
    links = {}
    link_chunks = header.split(', ')
    for link in link_chunks:
        url, rest = link.split('; ', 1)
        url = url.strip('<>')
        # Overkill, aeh.
        # Sample link header = '''<https://api.github.com/organizations/146304/repos?page=2>; rel="next", <https://api.github.com/organizations/146304/repos?page=4>; rel="last"'''
        other_keys = dict([ tuple([y.strip('"') for y in x.split('=')]) for x in rest.split('; ') ])
        links[other_keys['rel']] = url
    return links

def get_repos_from_org_listing(j):
    return [ x['name'] for x in j ]

def write_repos_to_output_file(repos):
    with open(OUTPUT_FILE, 'w') as f:
        for repo in repos:
            f.write("{}\n".format(repo))

def build_auth_header():
    token = os.environ.get('OAUTH_TOKEN', '').strip()
    return { 'Authorization': "token {}".format(token) }


def main():
    org_repos = []

    next_url = ORG_REPOS_URL
    while next_url:
        print("Fetching {}".format(next_url))

        # This get() can throw an exception if you hit the API request limit.
        # If that occurs, you'll get no listing, potentially after doing a
        # bunch of work. C'est la vie.
        r = requests.get(next_url, headers=build_auth_header())
        rj = r.json()

        org_repos += get_repos_from_org_listing(rj)
        link_header = r.headers['link']
        links = parse_github_http_link_header(link_header)
        next_url = links.get('next')

    org_repos.sort()
    write_repos_to_output_file(org_repos)
    return 0


if __name__ == "__main__":
    sys.exit(main())
