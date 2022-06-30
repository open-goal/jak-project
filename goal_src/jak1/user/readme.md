This directory holds the user profiles.

To make your own profile, create a new directory here with your username.
e.g. for username `mark` make a directory called `mark`
Inside that directory, create `user.gs` and `user.gc` files.
These are your own user scripts, loaded after the GOOS library and GOAL library respectively.

The rest of the directory can be used however you please!

To automatically log in as a specific user, create a `user.txt` file in this directory
which contains just the username you want to log in as. That way you don't have to
modify multiple scripts when you want to change users.

If you want to make your profile public, edit the .gitignore in this directory.
