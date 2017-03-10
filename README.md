# helm-org-snippets - A code snippet manager with Org and Helm #

This package collects code snippets under the inner-most Org heading. It
provides the following features:

- List all Org headings with at least a code snippet. Each entry can be visited
  for viewing.

- Insert a code snippet into the current buffer. The description text is
stripped and all code snippets are concantenated into a single snippet and then
it is insert into current buffer.

The snippets are retrieved by scanning the file list in `hos-org-file-list`. If
`org-wiki` is availabl, it automatically retrieves the list from `org-wiki`.

## Usage ##

Run the command `helm-org-snippets` to get all Org headings with snippets. Then, you can either:

- Run persistent action to view the snippet.
- Press `C-c i` to insert the tangled source code in a heading.

## Example ##

Suppose we have an org file with the following snippets:

```org
* Python
** Recipes
*** Find HTML tags with a Beautiful Soup object
API:

#+BEGIN_SRC python
  findAll(tag, attributes, recursive, text, limit, keywords)
  find(tag, attributes, recursive, text, keywords)
#+END_SRC

Example:

#+BEGIN_SRC python
     .findAll({"h1","h2","h3","h4","h5","h6"})
     .findAll("span", {"class":"green", "class":"red"})
     .findAll(text="the prince")
#+END_SRC

*** Parse HTML/XML with BeautifulSoup
1. Get the HTML structure of a URL:
   #+BEGIN_SRC python
     html = urlopen("http://en.wikipedia.org"+articleUrl)
   #+END_SRC

2. Use BeautifulSoup to parse:
     #+BEGIN_SRC python
       bsObj = BeautifulSoup(html, "lxml")
     #+END_SRC

3. Then, retrieve any data with Python e.g. regex:

#+BEGIN_SRC python
  bsObj.find("div", {"id":"bodyContent"}).findAll("a",href=re.compile("^(/wiki/)((?!:).)*$"))
#+END_SRC

```

Then, running the command `helm-org-snippets` will display it like this:

![helm-org-snippets](helm-org-snippets.png)

Finally, pressing `C-c i` inserts the raw code under a heading:

```python
findAll(tag, attributes, recursive, text, limit, keywords)
find(tag, attributes, recursive, text, keywords)
.findAll({"h1","h2","h3","h4","h5","h6"})
.findAll("span", {"class":"green", "class":"red"})
.findAll(text="the prince")
```
