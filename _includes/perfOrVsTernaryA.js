var valueA = true,
	loopCount = 1e8,
	testA,
	i


function executeTest () {

	console.time('testA')

	for (i = 0; i < loopCount; i++) {
		testA = valueA || 'test' + String(i)
		// testA = 'test' + String(i)
	}

	console.timeEnd('testA')
}

setTimeout(executeTest, 5000)
