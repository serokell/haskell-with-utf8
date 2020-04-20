# `with-utf8`

Get your IO right on the first try.

Reading files in Haskell is trickier than it could be due to the non-obvious
interactions between file encodings and system locale. This library is meant
to make it easy once and for all by providing “defaults” that make more sense
in the modern world.

See [this blog post][blog:post] for more details on why this library needs to
exists and an explanation of some of the opinionated decisions it is based on.

[blog:post]: https://serokell.io/blog/haskell-with-utf8


## Use

See the documentation on Hackage for details, this is a quick summary.

### Step 1: Get it

The [library is on Hackage][hackage:with-utf8],
go ahead and add it to the dependencies of your project.

[hackage:with-utf8]: https://hackage.haskell.org/package/with-utf8

### Step 2: Wrap your `main`

Import `withUtf8` from `Main.Utf8` and wrap it around your `main`:

```haskell
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 $
  {- ... your main function ... -}
```

This will make sure that if your program reads something from `stdin` or
outputs something to `stdout`/`stderr`, it will not fail with a runtime
error due to encoding issues.

### Step 3: Read files using UTF-8

If you are going to read a text file (to be precise, if you are going to open
a file in text mode), you’ll probably use `withFile`, `openFile`, or `readFile`.
Grab the first two from `System.IO.Utf8` or the latter from `Data.Text.IO.Utf8`.

_Note: it is best to import these modules qualified._

_Note: there is no `System.IO.Utf8.readFile` because it’s 2020 and
you should not read `String`s from files._

All these functions will make sure that the content will be treated as if it
was encoded in UTF-8.

If, for some reason, you really need to use `withFile`/`openFile` from `base`,
or you got your file handle from somewhere else, wrap the code that works
with it in a call to `withHandle` from `System.IO.Utf8`:

```haskell
import qualified System.IO as IO
import qualified System.IO.Utf8 as Utf8

doSomethingWithAFile :: IO.Handle -> IO ()
doSomethingWithAFile h = Utf8.withhandle h $ do
    {- ... work with the file ... -}
```

### Step 4: Write files using UTF-8

When writing a file either open it using `withFile`/`openFile` from
`System.IO.Utf8` or write to it directly with `writeFile` from
`Data.Text.IO.Utf8`.

_Note: it is best to import these modules qualified._

_Note: there is no `System.IO.Utf8.writeFile`._

If, for some reason, you really need to use `withFile`/`openFile` from `base`,
do the same as in the previous step.

## Troubleshooting

Locales are pretty straightforward, but some people might have their terminals
misconfigured for various reasons. To help troubleshoot any potential issues,
this package comes with a tool called `utf8-troubleshoot`.

This tool outputs some basic information about locale settings in the OS and
what they end up being mapped to in Haskell. If you are looking for help,
please, provide the output of this tool, or if you are helping someone,
ask them to run this tool and provide the output.

## Contributing

If you encounter any issues when using this library or have improvement ideas,
please open report in issue on GitHub. You are also very welcome to submit
pull request, if you feel like doing so.


## License

[MPL-2.0] © [Serokell]

[MPL-2.0]: https://spdx.org/licenses/MPL-2.0.html
[Serokell]: https://serokell.io/
