# convenience-functions
Convenience functions made on a whim 

Contents:
- **collate_output.R:** Wrap collate_output({}) around multi-line code chunks to capture and collate all messages generated in that chunk. Outputs all captured messages in order in the console. Convenient if your script generates a report, but you don't want to maintain it as an external file (e.g. with sink()).
