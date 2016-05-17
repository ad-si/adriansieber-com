var valueB = true,
	loopCount = 1e8,
	testB,
	i

function executeTest () {

	console.time('testB')

	for (i = 0; i < loopCount; i++) {
		testB = (typeof valueB !== "undefined" && valueB !== null) ?
			valueB :
			'test' + String(i)
		// testB = 'test' + String(i)
	}

	console.timeEnd('testB')
}

setTimeout(executeTest, 5000)
