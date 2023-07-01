|> : Fun<Task<a, err>, Fun<a, b>, Task<b, err>>
|> = \task, mapper -> (
    result <- task
    mapped = mapper(result)
    ok(mapped)
)

@ : Fun<Fun<a, b>, Fun<b, c>, Fun<a, c>>
@ = \f, g -> \x -> g(f(x))

url = `http://www.iana.org/assignments/media-types/application/vnd.api+json`

blah : Fun<Str, Str>
blah = \str -> `JSON {str}`

response <- request(url) |> (.body @ blah)
ok(response)
