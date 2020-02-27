# Code

### Static analysis of parsers

+ [Applicative example](./haskell/ApplicativeParser.hs)

+ [Profunctor example](./haskell/ProfunctorParser.hs)

+ [Monoid (lexer) example](./haskell/MonoidLexer.hs)

Monadic parser combinators are popular, but this comes at the expense of static analysis.

For instance, imagine you're parsing a language with a certain number of keywords like `let`, `case`, etc. You also have a variable parser that you'd like to forbid from parsing those keywords. With monadic parser combinators you have to maintain a list of keywords in your source code to be used by the variable parser, which could go out of sync with the actual keywords your language contains.

However, if you restrict your parser to just Applicative (and Alternative) you don't have to track that list separately, but can assemble it as you combine parsers together with `<*>`.

Applicative+Alternative is pretty powerful, but doesn't give later parsers access to the results of earlier ones. This can be done with Category based parsers, which when equipped with Profunctor and Strong begins to come close to\* the power of Monadic parsers.

Examples of Applicative and Profunctor based parsers are provided, as well as a Monoid-based lexer for a simpler example if you find the others confusing.

\* Or reaches? I'm not sure.

# Issue tracker

Used by me to track tech questions I have.

I prefer this to Q/A sites because they may edit or delete questions. I can still crosspost them, but this way I can keep the originals online as well.

It's also better than timeline based sites like twitter because they stay prominent in the UI until I close them.