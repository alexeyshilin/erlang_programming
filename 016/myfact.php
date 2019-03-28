<?php

function factorial($n)
{
    if ($n == 0) {
        return 1;
    } else {
        return $n * factorial($n - 1);
    }
}

$stdin = fopen('php://stdin', 'r');

//$data = stream_get_contents($stdin, 1);
$data = trim(fgets(STDIN));

$result = factorial((int) $data );

print(sprintf("PHP language. %d\n", $result));
