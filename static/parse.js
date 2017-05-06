
const NumberExp = /[+-]?[0-9]+[.]?[0-9]([eE][+-])?[0-9]*/;
const WhiteSpaceExp = /\s/;

function arrayToList(obj) {
	if (typeof(obj) === 'object') {
		return obj.children.map(this.arrayToList);
	}

	return obj;
}

function parseLisp(src, idx) {
	if (idx === undefined) idx = 0;
	let acc = "";
	let res = null;

	for (; src.length > idx; idx++) {
		switch (src[idx]) {
			case '#':
				if (acc !== '') return [acc, idx];
				[res, idx] = parseSharp(src, idx+1);
				return [res, idx];
				break;
			case '(':
				if (acc !== '') return [acc, idx];
				[res, idx] = parseList(src, idx+1);
				return [res, idx];
				break;
			case '"':
				if (acc !== '') return [acc, idx];
				[res, idx] = parseString(src, idx+1);
				return [res, idx];
				break;
			default:
				acc += src[idx];
				break;
		}
	}

	if (acc !== '') res = parseSymbol(acc);

	return [res, idx];
}

function parseSymbol(src) {
	let n = src.match(NumberExp);
	if (n && n[0].length === src.length) {
		return parseFloat(src);
	} else {
		return src;
	}
}

function parseList(src, idx) {
	let acc = "";
	let res = [];

	while (src.length > idx) {
		switch (src[idx]) {
			case '#':
				[tmp, idx] = parseSharp(src, idx+1);
				res.push(tmp);
				break;
			case '(':
				[tmp, idx] = parseList(src, idx+1);
				res.push(tmp);
				break;
			case '"':
				[tmp, idx] = parseString(src, idx+1);
				res.push(tmp);
				break;
			case ')':
				if (acc !== '') res.push(parseSymbol(acc));
				return [res, idx+1];
				break;
			default:
				if (src[idx].match(WhiteSpaceExp)) {
					if (acc !== '') {
						res.push(parseSymbol(acc));
						acc = '';
					}
				} else {
					acc += src[idx]
				}
				idx++;
				break;
		}
	}

	console.log('uncorrespond');
	return [res, idx];
}

function parseSharp(src, idx) {
	let acc = "";
	let res = {mark: null, children: []};

	for (; src.length > idx; idx++) {
		switch (src[idx]) {
			case '(':
				res.mark = '#'+acc;
				[res.children, idx] = parseList(src, idx+1);
				return [res, idx];
				break;
			default:
				if (src[idx].match(WhiteSpaceExp)) {
					return ['#'+acc, idx+1];
				}
				acc += src[idx];
				break;
		}
	}

	return ['#'+acc, idx];
}

function parseString(src, idx) {
	let acc = "";

	for (; src.length > idx; idx++) {
		switch (src[idx]) {
			case '"':
				return [acc, idx+1];
				break;
			default:
				acc += src[idx];
				break;
		}
	}

	return [acc, idx];
}
