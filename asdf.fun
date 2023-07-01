|> : \Task<a, err>, (\a -> b) -> Task<b, err>
|> = \task, mapper -> (
    result <- task
    mapped = mapper(result)
    ok(mapped)
)

@ : \(\a -> b), (\b -> c) -> (\a -> c)
@ = \f, g -> \x -> g(f(x))

url = `http://www.iana.org/assignments/media-types/application/vnd.api+json`

blah : \Str -> Str
blah = \str -> `JSON {str}`

response <- request(url) |> (.body @ blah)
ok(response)
