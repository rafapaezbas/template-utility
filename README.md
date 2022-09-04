# Template-utility

Generates new file from template. 

## Usage

Template format:

```
template-utility /path/to/file1.txt
```

file1.txt

```
This is file 1. It includes tags that will be replaced with content.

{file2.txt}
```

file2.txt (in the same folder as file1.txt)

```
This is the content of file 2.
```

Result in file1.txt_:

```
This is file 1. It includes tags that will be replaced with content.

This is the content of file 2.
```

