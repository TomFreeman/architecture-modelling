rm -r ./out

dotnet fable watch --lang Python --cwd ./src/Model --outDir "$PWD/out/Model"
