#!/bin/bash
# Download files for a list of FASTA IDs given in the passed file

filename="$1"

while read -r line
do
  id="$line"
  file="$id.fasta"
  url="http://www.uniprot.org/uniprot/$file"

  echo "Getting FASTA record from $url"

  # The -L follows redirects (seemingly common in the FASTA db)
  curl -L $url --output "fasta/$file"

done < "$filename"

