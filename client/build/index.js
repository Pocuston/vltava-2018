
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

//import Native.List //

var _elm_lang$core$Native_Array = function() {

// A RRB-Tree has two distinct data types.
// Leaf -> "height"  is always 0
//         "table"   is an array of elements
// Node -> "height"  is always greater than 0
//         "table"   is an array of child nodes
//         "lengths" is an array of accumulated lengths of the child nodes

// M is the maximal table size. 32 seems fast. E is the allowed increase
// of search steps when concatting to find an index. Lower values will
// decrease balancing, but will increase search steps.
var M = 32;
var E = 2;

// An empty array.
var empty = {
	ctor: '_Array',
	height: 0,
	table: []
};


function get(i, array)
{
	if (i < 0 || i >= length(array))
	{
		throw new Error(
			'Index ' + i + ' is out of range. Check the length of ' +
			'your array first or use getMaybe or getWithDefault.');
	}
	return unsafeGet(i, array);
}


function unsafeGet(i, array)
{
	for (var x = array.height; x > 0; x--)
	{
		var slot = i >> (x * 5);
		while (array.lengths[slot] <= i)
		{
			slot++;
		}
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array = array.table[slot];
	}
	return array.table[i];
}


// Sets the value at the index i. Only the nodes leading to i will get
// copied and updated.
function set(i, item, array)
{
	if (i < 0 || length(array) <= i)
	{
		return array;
	}
	return unsafeSet(i, item, array);
}


function unsafeSet(i, item, array)
{
	array = nodeCopy(array);

	if (array.height === 0)
	{
		array.table[i] = item;
	}
	else
	{
		var slot = getSlot(i, array);
		if (slot > 0)
		{
			i -= array.lengths[slot - 1];
		}
		array.table[slot] = unsafeSet(i, item, array.table[slot]);
	}
	return array;
}


function initialize(len, f)
{
	if (len <= 0)
	{
		return empty;
	}
	var h = Math.floor( Math.log(len) / Math.log(M) );
	return initialize_(f, h, 0, len);
}

function initialize_(f, h, from, to)
{
	if (h === 0)
	{
		var table = new Array((to - from) % (M + 1));
		for (var i = 0; i < table.length; i++)
		{
		  table[i] = f(from + i);
		}
		return {
			ctor: '_Array',
			height: 0,
			table: table
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

function fromList(list)
{
	if (list.ctor === '[]')
	{
		return empty;
	}

	// Allocate M sized blocks (table) and write list elements to it.
	var table = new Array(M);
	var nodes = [];
	var i = 0;

	while (list.ctor !== '[]')
	{
		table[i] = list._0;
		list = list._1;
		i++;

		// table is full, so we can push a leaf containing it into the
		// next node.
		if (i === M)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table
			};
			fromListPush(leaf, nodes);
			table = new Array(M);
			i = 0;
		}
	}

	// Maybe there is something left on the table.
	if (i > 0)
	{
		var leaf = {
			ctor: '_Array',
			height: 0,
			table: table.splice(0, i)
		};
		fromListPush(leaf, nodes);
	}

	// Go through all of the nodes and eventually push them into higher nodes.
	for (var h = 0; h < nodes.length - 1; h++)
	{
		if (nodes[h].table.length > 0)
		{
			fromListPush(nodes[h], nodes);
		}
	}

	var head = nodes[nodes.length - 1];
	if (head.height > 0 && head.table.length === 1)
	{
		return head.table[0];
	}
	else
	{
		return head;
	}
}

// Push a node into a higher node as a child.
function fromListPush(toPush, nodes)
{
	var h = toPush.height;

	// Maybe the node on this height does not exist.
	if (nodes.length === h)
	{
		var node = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
		nodes.push(node);
	}

	nodes[h].table.push(toPush);
	var len = length(toPush);
	if (nodes[h].lengths.length > 0)
	{
		len += nodes[h].lengths[nodes[h].lengths.length - 1];
	}
	nodes[h].lengths.push(len);

	if (nodes[h].table.length === M)
	{
		fromListPush(nodes[h], nodes);
		nodes[h] = {
			ctor: '_Array',
			height: h + 1,
			table: [],
			lengths: []
		};
	}
}

// Pushes an item via push_ to the bottom right of a tree.
function push(item, a)
{
	var pushed = push_(item, a);
	if (pushed !== null)
	{
		return pushed;
	}

	var newTree = create(item, a.height);
	return siblise(a, newTree);
}

// Recursively tries to push an item to the bottom-right most
// tree possible. If there is no space left for the item,
// null will be returned.
function push_(item, a)
{
	// Handle resursion stop at leaf level.
	if (a.height === 0)
	{
		if (a.table.length < M)
		{
			var newA = {
				ctor: '_Array',
				height: 0,
				table: a.table.slice()
			};
			newA.table.push(item);
			return newA;
		}
		else
		{
		  return null;
		}
	}

	// Recursively push
	var pushed = push_(item, botRight(a));

	// There was space in the bottom right tree, so the slot will
	// be updated.
	if (pushed !== null)
	{
		var newA = nodeCopy(a);
		newA.table[newA.table.length - 1] = pushed;
		newA.lengths[newA.lengths.length - 1]++;
		return newA;
	}

	// When there was no space left, check if there is space left
	// for a new slot with a tree which contains only the item
	// at the bottom.
	if (a.table.length < M)
	{
		var newSlot = create(item, a.height - 1);
		var newA = nodeCopy(a);
		newA.table.push(newSlot);
		newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
		return newA;
	}
	else
	{
		return null;
	}
}

// Converts an array into a list of elements.
function toList(a)
{
	return toList_(_elm_lang$core$Native_List.Nil, a);
}

function toList_(list, a)
{
	for (var i = a.table.length - 1; i >= 0; i--)
	{
		list =
			a.height === 0
				? _elm_lang$core$Native_List.Cons(a.table[i], list)
				: toList_(list, a.table[i]);
	}
	return list;
}

// Maps a function over the elements of an array.
function map(f, a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? f(a.table[i])
				: map(f, a.table[i]);
	}
	return newA;
}

// Maps a function over the elements with their index as first argument.
function indexedMap(f, a)
{
	return indexedMap_(f, a, 0);
}

function indexedMap_(f, a, from)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: new Array(a.table.length)
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths;
	}
	for (var i = 0; i < a.table.length; i++)
	{
		newA.table[i] =
			a.height === 0
				? A2(f, from + i, a.table[i])
				: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
	}
	return newA;
}

function foldl(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = 0; i < a.table.length; i++)
		{
			b = foldl(f, b, a.table[i]);
		}
	}
	return b;
}

function foldr(f, b, a)
{
	if (a.height === 0)
	{
		for (var i = a.table.length; i--; )
		{
			b = A2(f, a.table[i], b);
		}
	}
	else
	{
		for (var i = a.table.length; i--; )
		{
			b = foldr(f, b, a.table[i]);
		}
	}
	return b;
}

// TODO: currently, it slices the right, then the left. This can be
// optimized.
function slice(from, to, a)
{
	if (from < 0)
	{
		from += length(a);
	}
	if (to < 0)
	{
		to += length(a);
	}
	return sliceLeft(from, sliceRight(to, a));
}

function sliceRight(to, a)
{
	if (to === length(a))
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(0, to);
		return newA;
	}

	// Slice the right recursively.
	var right = getSlot(to, a);
	var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (right === 0)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(0, right),
		lengths: a.lengths.slice(0, right)
	};
	if (sliced.table.length > 0)
	{
		newA.table[right] = sliced;
		newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
	}
	return newA;
}

function sliceLeft(from, a)
{
	if (from === 0)
	{
		return a;
	}

	// Handle leaf level.
	if (a.height === 0)
	{
		var newA = { ctor:'_Array', height:0 };
		newA.table = a.table.slice(from, a.table.length + 1);
		return newA;
	}

	// Slice the left recursively.
	var left = getSlot(from, a);
	var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

	// Maybe the a node is not even needed, as sliced contains the whole slice.
	if (left === a.table.length - 1)
	{
		return sliced;
	}

	// Create new node.
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice(left, a.table.length + 1),
		lengths: new Array(a.table.length - left)
	};
	newA.table[0] = sliced;
	var len = 0;
	for (var i = 0; i < newA.table.length; i++)
	{
		len += length(newA.table[i]);
		newA.lengths[i] = len;
	}

	return newA;
}

// Appends two trees.
function append(a,b)
{
	if (a.table.length === 0)
	{
		return b;
	}
	if (b.table.length === 0)
	{
		return a;
	}

	var c = append_(a, b);

	// Check if both nodes can be crunshed together.
	if (c[0].table.length + c[1].table.length <= M)
	{
		if (c[0].table.length === 0)
		{
			return c[1];
		}
		if (c[1].table.length === 0)
		{
			return c[0];
		}

		// Adjust .table and .lengths
		c[0].table = c[0].table.concat(c[1].table);
		if (c[0].height > 0)
		{
			var len = length(c[0]);
			for (var i = 0; i < c[1].lengths.length; i++)
			{
				c[1].lengths[i] += len;
			}
			c[0].lengths = c[0].lengths.concat(c[1].lengths);
		}

		return c[0];
	}

	if (c[0].height > 0)
	{
		var toRemove = calcToRemove(a, b);
		if (toRemove > E)
		{
			c = shuffle(c[0], c[1], toRemove);
		}
	}

	return siblise(c[0], c[1]);
}

// Returns an array of two nodes; right and left. One node _may_ be empty.
function append_(a, b)
{
	if (a.height === 0 && b.height === 0)
	{
		return [a, b];
	}

	if (a.height !== 1 || b.height !== 1)
	{
		if (a.height === b.height)
		{
			a = nodeCopy(a);
			b = nodeCopy(b);
			var appended = append_(botRight(a), botLeft(b));

			insertRight(a, appended[1]);
			insertLeft(b, appended[0]);
		}
		else if (a.height > b.height)
		{
			a = nodeCopy(a);
			var appended = append_(botRight(a), b);

			insertRight(a, appended[0]);
			b = parentise(appended[1], appended[1].height + 1);
		}
		else
		{
			b = nodeCopy(b);
			var appended = append_(a, botLeft(b));

			var left = appended[0].table.length === 0 ? 0 : 1;
			var right = left === 0 ? 1 : 0;
			insertLeft(b, appended[left]);
			a = parentise(appended[right], appended[right].height + 1);
		}
	}

	// Check if balancing is needed and return based on that.
	if (a.table.length === 0 || b.table.length === 0)
	{
		return [a, b];
	}

	var toRemove = calcToRemove(a, b);
	if (toRemove <= E)
	{
		return [a, b];
	}
	return shuffle(a, b, toRemove);
}

// Helperfunctions for append_. Replaces a child node at the side of the parent.
function insertRight(parent, node)
{
	var index = parent.table.length - 1;
	parent.table[index] = node;
	parent.lengths[index] = length(node);
	parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
}

function insertLeft(parent, node)
{
	if (node.table.length > 0)
	{
		parent.table[0] = node;
		parent.lengths[0] = length(node);

		var len = length(parent.table[0]);
		for (var i = 1; i < parent.lengths.length; i++)
		{
			len += length(parent.table[i]);
			parent.lengths[i] = len;
		}
	}
	else
	{
		parent.table.shift();
		for (var i = 1; i < parent.lengths.length; i++)
		{
			parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
		}
		parent.lengths.shift();
	}
}

// Returns the extra search steps for E. Refer to the paper.
function calcToRemove(a, b)
{
	var subLengths = 0;
	for (var i = 0; i < a.table.length; i++)
	{
		subLengths += a.table[i].table.length;
	}
	for (var i = 0; i < b.table.length; i++)
	{
		subLengths += b.table[i].table.length;
	}

	var toRemove = a.table.length + b.table.length;
	return toRemove - (Math.floor((subLengths - 1) / M) + 1);
}

// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
function get2(a, b, index)
{
	return index < a.length
		? a[index]
		: b[index - a.length];
}

function set2(a, b, index, value)
{
	if (index < a.length)
	{
		a[index] = value;
	}
	else
	{
		b[index - a.length] = value;
	}
}

function saveSlot(a, b, index, slot)
{
	set2(a.table, b.table, index, slot);

	var l = (index === 0 || index === a.lengths.length)
		? 0
		: get2(a.lengths, a.lengths, index - 1);

	set2(a.lengths, b.lengths, index, l + length(slot));
}

// Creates a node or leaf with a given length at their arrays for perfomance.
// Is only used by shuffle.
function createNode(h, length)
{
	if (length < 0)
	{
		length = 0;
	}
	var a = {
		ctor: '_Array',
		height: h,
		table: new Array(length)
	};
	if (h > 0)
	{
		a.lengths = new Array(length);
	}
	return a;
}

// Returns an array of two balanced nodes.
function shuffle(a, b, toRemove)
{
	var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
	var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

	// Skip the slots with size M. More precise: copy the slot references
	// to the new node
	var read = 0;
	while (get2(a.table, b.table, read).table.length % M === 0)
	{
		set2(newA.table, newB.table, read, get2(a.table, b.table, read));
		set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
		read++;
	}

	// Pulling items from left to right, caching in a slot before writing
	// it into the new nodes.
	var write = read;
	var slot = new createNode(a.height - 1, 0);
	var from = 0;

	// If the current slot is still containing data, then there will be at
	// least one more write, so we do not break this loop yet.
	while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
	{
		// Find out the max possible items for copying.
		var source = get2(a.table, b.table, read);
		var to = Math.min(M - slot.table.length, source.table.length);

		// Copy and adjust size table.
		slot.table = slot.table.concat(source.table.slice(from, to));
		if (slot.height > 0)
		{
			var len = slot.lengths.length;
			for (var i = len; i < len + to - from; i++)
			{
				slot.lengths[i] = length(slot.table[i]);
				slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
			}
		}

		from += to;

		// Only proceed to next slots[i] if the current one was
		// fully copied.
		if (source.table.length <= to)
		{
			read++; from = 0;
		}

		// Only create a new slot if the current one is filled up.
		if (slot.table.length === M)
		{
			saveSlot(newA, newB, write, slot);
			slot = createNode(a.height - 1, 0);
			write++;
		}
	}

	// Cleanup after the loop. Copy the last slot into the new nodes.
	if (slot.table.length > 0)
	{
		saveSlot(newA, newB, write, slot);
		write++;
	}

	// Shift the untouched slots to the left
	while (read < a.table.length + b.table.length )
	{
		saveSlot(newA, newB, write, get2(a.table, b.table, read));
		read++;
		write++;
	}

	return [newA, newB];
}

// Navigation functions
function botRight(a)
{
	return a.table[a.table.length - 1];
}
function botLeft(a)
{
	return a.table[0];
}

// Copies a node for updating. Note that you should not use this if
// only updating only one of "table" or "lengths" for performance reasons.
function nodeCopy(a)
{
	var newA = {
		ctor: '_Array',
		height: a.height,
		table: a.table.slice()
	};
	if (a.height > 0)
	{
		newA.lengths = a.lengths.slice();
	}
	return newA;
}

// Returns how many items are in the tree.
function length(array)
{
	if (array.height === 0)
	{
		return array.table.length;
	}
	else
	{
		return array.lengths[array.lengths.length - 1];
	}
}

// Calculates in which slot of "table" the item probably is, then
// find the exact slot via forward searching in  "lengths". Returns the index.
function getSlot(i, a)
{
	var slot = i >> (5 * a.height);
	while (a.lengths[slot] <= i)
	{
		slot++;
	}
	return slot;
}

// Recursively creates a tree with a given height containing
// only the given item.
function create(item, h)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: [item]
		};
	}
	return {
		ctor: '_Array',
		height: h,
		table: [create(item, h - 1)],
		lengths: [1]
	};
}

// Recursively creates a tree that contains the given tree.
function parentise(tree, h)
{
	if (h === tree.height)
	{
		return tree;
	}

	return {
		ctor: '_Array',
		height: h,
		table: [parentise(tree, h - 1)],
		lengths: [length(tree)]
	};
}

// Emphasizes blood brotherhood beneath two trees.
function siblise(a, b)
{
	return {
		ctor: '_Array',
		height: a.height + 1,
		table: [a, b],
		lengths: [length(a), length(a) + length(b)]
	};
}

function toJSArray(a)
{
	var jsArray = new Array(length(a));
	toJSArray_(jsArray, 0, a);
	return jsArray;
}

function toJSArray_(jsArray, i, a)
{
	for (var t = 0; t < a.table.length; t++)
	{
		if (a.height === 0)
		{
			jsArray[i + t] = a.table[t];
		}
		else
		{
			var inc = t === 0 ? 0 : a.lengths[t - 1];
			toJSArray_(jsArray, i + inc, a.table[t]);
		}
	}
}

function fromJSArray(jsArray)
{
	if (jsArray.length === 0)
	{
		return empty;
	}
	var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
	return fromJSArray_(jsArray, h, 0, jsArray.length);
}

function fromJSArray_(jsArray, h, from, to)
{
	if (h === 0)
	{
		return {
			ctor: '_Array',
			height: 0,
			table: jsArray.slice(from, to)
		};
	}

	var step = Math.pow(M, h);
	var table = new Array(Math.ceil((to - from) / step));
	var lengths = new Array(table.length);
	for (var i = 0; i < table.length; i++)
	{
		table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
		lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
	}
	return {
		ctor: '_Array',
		height: h,
		table: table,
		lengths: lengths
	};
}

return {
	empty: empty,
	fromList: fromList,
	toList: toList,
	initialize: F2(initialize),
	append: F2(append),
	push: F2(push),
	slice: F3(slice),
	get: F2(get),
	set: F3(set),
	map: F2(map),
	indexedMap: F2(indexedMap),
	foldl: F3(foldl),
	foldr: F3(foldr),
	length: length,

	toJSArray: toJSArray,
	fromJSArray: fromJSArray
};

}();
//import Native.Utils //

var _elm_lang$core$Native_Basics = function() {

function div(a, b)
{
	return (a / b) | 0;
}
function rem(a, b)
{
	return a % b;
}
function mod(a, b)
{
	if (b === 0)
	{
		throw new Error('Cannot perform mod 0. Division by zero error.');
	}
	var r = a % b;
	var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

	return m === b ? 0 : m;
}
function logBase(base, n)
{
	return Math.log(n) / Math.log(base);
}
function negate(n)
{
	return -n;
}
function abs(n)
{
	return n < 0 ? -n : n;
}

function min(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) < 0 ? a : b;
}
function max(a, b)
{
	return _elm_lang$core$Native_Utils.cmp(a, b) > 0 ? a : b;
}
function clamp(lo, hi, n)
{
	return _elm_lang$core$Native_Utils.cmp(n, lo) < 0
		? lo
		: _elm_lang$core$Native_Utils.cmp(n, hi) > 0
			? hi
			: n;
}

var ord = ['LT', 'EQ', 'GT'];

function compare(x, y)
{
	return { ctor: ord[_elm_lang$core$Native_Utils.cmp(x, y) + 1] };
}

function xor(a, b)
{
	return a !== b;
}
function not(b)
{
	return !b;
}
function isInfinite(n)
{
	return n === Infinity || n === -Infinity;
}

function truncate(n)
{
	return n | 0;
}

function degrees(d)
{
	return d * Math.PI / 180;
}
function turns(t)
{
	return 2 * Math.PI * t;
}
function fromPolar(point)
{
	var r = point._0;
	var t = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
}
function toPolar(point)
{
	var x = point._0;
	var y = point._1;
	return _elm_lang$core$Native_Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
}

return {
	div: F2(div),
	rem: F2(rem),
	mod: F2(mod),

	pi: Math.PI,
	e: Math.E,
	cos: Math.cos,
	sin: Math.sin,
	tan: Math.tan,
	acos: Math.acos,
	asin: Math.asin,
	atan: Math.atan,
	atan2: F2(Math.atan2),

	degrees: degrees,
	turns: turns,
	fromPolar: fromPolar,
	toPolar: toPolar,

	sqrt: Math.sqrt,
	logBase: F2(logBase),
	negate: negate,
	abs: abs,
	min: F2(min),
	max: F2(max),
	clamp: F3(clamp),
	compare: F2(compare),

	xor: F2(xor),
	not: not,

	truncate: truncate,
	ceiling: Math.ceil,
	floor: Math.floor,
	round: Math.round,
	toFloat: function(x) { return x; },
	isNaN: isNaN,
	isInfinite: isInfinite
};

}();
//import //

var _elm_lang$core$Native_Utils = function() {

// COMPARISONS

function eq(x, y)
{
	var stack = [];
	var isEqual = eqHelp(x, y, 0, stack);
	var pair;
	while (isEqual && (pair = stack.pop()))
	{
		isEqual = eqHelp(pair.x, pair.y, 0, stack);
	}
	return isEqual;
}


function eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push({ x: x, y: y });
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object')
	{
		if (typeof x === 'function')
		{
			throw new Error(
				'Trying to use `(==)` on functions. There is no way to know if functions are "the same" in the Elm sense.'
				+ ' Read more about this at http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#=='
				+ ' which describes why it is this way and what the better version will look like.'
			);
		}
		return false;
	}

	if (x === null || y === null)
	{
		return false
	}

	if (x instanceof Date)
	{
		return x.getTime() === y.getTime();
	}

	if (!('ctor' in x))
	{
		for (var key in x)
		{
			if (!eqHelp(x[key], y[key], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	// convert Dicts and Sets to lists
	if (x.ctor === 'RBNode_elm_builtin' || x.ctor === 'RBEmpty_elm_builtin')
	{
		x = _elm_lang$core$Dict$toList(x);
		y = _elm_lang$core$Dict$toList(y);
	}
	if (x.ctor === 'Set_elm_builtin')
	{
		x = _elm_lang$core$Set$toList(x);
		y = _elm_lang$core$Set$toList(y);
	}

	// check if lists are equal without recursion
	if (x.ctor === '::')
	{
		var a = x;
		var b = y;
		while (a.ctor === '::' && b.ctor === '::')
		{
			if (!eqHelp(a._0, b._0, depth + 1, stack))
			{
				return false;
			}
			a = a._1;
			b = b._1;
		}
		return a.ctor === b.ctor;
	}

	// check if Arrays are equal
	if (x.ctor === '_Array')
	{
		var xs = _elm_lang$core$Native_Array.toJSArray(x);
		var ys = _elm_lang$core$Native_Array.toJSArray(y);
		if (xs.length !== ys.length)
		{
			return false;
		}
		for (var i = 0; i < xs.length; i++)
		{
			if (!eqHelp(xs[i], ys[i], depth + 1, stack))
			{
				return false;
			}
		}
		return true;
	}

	if (!eqHelp(x.ctor, y.ctor, depth + 1, stack))
	{
		return false;
	}

	for (var key in x)
	{
		if (!eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

var LT = -1, EQ = 0, GT = 1;

function cmp(x, y)
{
	if (typeof x !== 'object')
	{
		return x === y ? EQ : x < y ? LT : GT;
	}

	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? EQ : a < b ? LT : GT;
	}

	if (x.ctor === '::' || x.ctor === '[]')
	{
		while (x.ctor === '::' && y.ctor === '::')
		{
			var ord = cmp(x._0, y._0);
			if (ord !== EQ)
			{
				return ord;
			}
			x = x._1;
			y = y._1;
		}
		return x.ctor === y.ctor ? EQ : x.ctor === '[]' ? LT : GT;
	}

	if (x.ctor.slice(0, 6) === '_Tuple')
	{
		var ord;
		var n = x.ctor.slice(6) - 0;
		var err = 'cannot compare tuples with more than 6 elements.';
		if (n === 0) return EQ;
		if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
		if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
		if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
		if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
		if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
		if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
		if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
		return EQ;
	}

	throw new Error(
		'Comparison error: comparison is only defined on ints, '
		+ 'floats, times, chars, strings, lists of comparable values, '
		+ 'and tuples of comparable values.'
	);
}


// COMMON VALUES

var Tuple0 = {
	ctor: '_Tuple0'
};

function Tuple2(x, y)
{
	return {
		ctor: '_Tuple2',
		_0: x,
		_1: y
	};
}

function chr(c)
{
	return new String(c);
}


// GUID

var count = 0;
function guid(_)
{
	return count++;
}


// RECORDS

function update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


//// LIST STUFF ////

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return {
		ctor: '::',
		_0: hd,
		_1: tl
	};
}

function append(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (xs.ctor === '[]')
	{
		return ys;
	}
	var root = Cons(xs._0, Nil);
	var curr = root;
	xs = xs._1;
	while (xs.ctor !== '[]')
	{
		curr._1 = Cons(xs._0, Nil);
		xs = xs._1;
		curr = curr._1;
	}
	curr._1 = ys;
	return root;
}


// CRASHES

function crash(moduleName, region)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function crashCase(moduleName, region, value)
{
	return function(message) {
		throw new Error(
			'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
			+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
			+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
			+ 'The message provided by the code author is:\n\n    '
			+ message
		);
	};
}

function regionToString(region)
{
	if (region.start.line == region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'between lines ' + region.start.line + ' and ' + region.end.line;
}


// TO STRING

function toString(v)
{
	var type = typeof v;
	if (type === 'function')
	{
		return '<function>';
	}

	if (type === 'boolean')
	{
		return v ? 'True' : 'False';
	}

	if (type === 'number')
	{
		return v + '';
	}

	if (v instanceof String)
	{
		return '\'' + addSlashes(v, true) + '\'';
	}

	if (type === 'string')
	{
		return '"' + addSlashes(v, false) + '"';
	}

	if (v === null)
	{
		return 'null';
	}

	if (type === 'object' && 'ctor' in v)
	{
		var ctorStarter = v.ctor.substring(0, 5);

		if (ctorStarter === '_Tupl')
		{
			var output = [];
			for (var k in v)
			{
				if (k === 'ctor') continue;
				output.push(toString(v[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (ctorStarter === '_Task')
		{
			return '<task>'
		}

		if (v.ctor === '_Array')
		{
			var list = _elm_lang$core$Array$toList(v);
			return 'Array.fromList ' + toString(list);
		}

		if (v.ctor === '<decoder>')
		{
			return '<decoder>';
		}

		if (v.ctor === '_Process')
		{
			return '<process:' + v.id + '>';
		}

		if (v.ctor === '::')
		{
			var output = '[' + toString(v._0);
			v = v._1;
			while (v.ctor === '::')
			{
				output += ',' + toString(v._0);
				v = v._1;
			}
			return output + ']';
		}

		if (v.ctor === '[]')
		{
			return '[]';
		}

		if (v.ctor === 'Set_elm_builtin')
		{
			return 'Set.fromList ' + toString(_elm_lang$core$Set$toList(v));
		}

		if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin')
		{
			return 'Dict.fromList ' + toString(_elm_lang$core$Dict$toList(v));
		}

		var output = '';
		for (var i in v)
		{
			if (i === 'ctor') continue;
			var str = toString(v[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return v.ctor + output;
	}

	if (type === 'object')
	{
		if (v instanceof Date)
		{
			return '<' + v.toString() + '>';
		}

		if (v.elm_web_socket)
		{
			return '<websocket>';
		}

		var output = [];
		for (var k in v)
		{
			output.push(k + ' = ' + toString(v[k]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return '<internal structure>';
}

function addSlashes(str, isChar)
{
	var s = str.replace(/\\/g, '\\\\')
			  .replace(/\n/g, '\\n')
			  .replace(/\t/g, '\\t')
			  .replace(/\r/g, '\\r')
			  .replace(/\v/g, '\\v')
			  .replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}


return {
	eq: eq,
	cmp: cmp,
	Tuple0: Tuple0,
	Tuple2: Tuple2,
	chr: chr,
	update: update,
	guid: guid,

	append: F2(append),

	crash: crash,
	crashCase: crashCase,

	toString: toString
};

}();
var _elm_lang$core$Basics$never = function (_p0) {
	never:
	while (true) {
		var _p1 = _p0;
		var _v1 = _p1._0;
		_p0 = _v1;
		continue never;
	}
};
var _elm_lang$core$Basics$uncurry = F2(
	function (f, _p2) {
		var _p3 = _p2;
		return A2(f, _p3._0, _p3._1);
	});
var _elm_lang$core$Basics$curry = F3(
	function (f, a, b) {
		return f(
			{ctor: '_Tuple2', _0: a, _1: b});
	});
var _elm_lang$core$Basics$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var _elm_lang$core$Basics$always = F2(
	function (a, _p4) {
		return a;
	});
var _elm_lang$core$Basics$identity = function (x) {
	return x;
};
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<|'] = F2(
	function (f, x) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['|>'] = F2(
	function (x, f) {
		return f(x);
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>>'] = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<<'] = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['++'] = _elm_lang$core$Native_Utils.append;
var _elm_lang$core$Basics$toString = _elm_lang$core$Native_Utils.toString;
var _elm_lang$core$Basics$isInfinite = _elm_lang$core$Native_Basics.isInfinite;
var _elm_lang$core$Basics$isNaN = _elm_lang$core$Native_Basics.isNaN;
var _elm_lang$core$Basics$toFloat = _elm_lang$core$Native_Basics.toFloat;
var _elm_lang$core$Basics$ceiling = _elm_lang$core$Native_Basics.ceiling;
var _elm_lang$core$Basics$floor = _elm_lang$core$Native_Basics.floor;
var _elm_lang$core$Basics$truncate = _elm_lang$core$Native_Basics.truncate;
var _elm_lang$core$Basics$round = _elm_lang$core$Native_Basics.round;
var _elm_lang$core$Basics$not = _elm_lang$core$Native_Basics.not;
var _elm_lang$core$Basics$xor = _elm_lang$core$Native_Basics.xor;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['||'] = _elm_lang$core$Native_Basics.or;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['&&'] = _elm_lang$core$Native_Basics.and;
var _elm_lang$core$Basics$max = _elm_lang$core$Native_Basics.max;
var _elm_lang$core$Basics$min = _elm_lang$core$Native_Basics.min;
var _elm_lang$core$Basics$compare = _elm_lang$core$Native_Basics.compare;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>='] = _elm_lang$core$Native_Basics.ge;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<='] = _elm_lang$core$Native_Basics.le;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['>'] = _elm_lang$core$Native_Basics.gt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['<'] = _elm_lang$core$Native_Basics.lt;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/='] = _elm_lang$core$Native_Basics.neq;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['=='] = _elm_lang$core$Native_Basics.eq;
var _elm_lang$core$Basics$e = _elm_lang$core$Native_Basics.e;
var _elm_lang$core$Basics$pi = _elm_lang$core$Native_Basics.pi;
var _elm_lang$core$Basics$clamp = _elm_lang$core$Native_Basics.clamp;
var _elm_lang$core$Basics$logBase = _elm_lang$core$Native_Basics.logBase;
var _elm_lang$core$Basics$abs = _elm_lang$core$Native_Basics.abs;
var _elm_lang$core$Basics$negate = _elm_lang$core$Native_Basics.negate;
var _elm_lang$core$Basics$sqrt = _elm_lang$core$Native_Basics.sqrt;
var _elm_lang$core$Basics$atan2 = _elm_lang$core$Native_Basics.atan2;
var _elm_lang$core$Basics$atan = _elm_lang$core$Native_Basics.atan;
var _elm_lang$core$Basics$asin = _elm_lang$core$Native_Basics.asin;
var _elm_lang$core$Basics$acos = _elm_lang$core$Native_Basics.acos;
var _elm_lang$core$Basics$tan = _elm_lang$core$Native_Basics.tan;
var _elm_lang$core$Basics$sin = _elm_lang$core$Native_Basics.sin;
var _elm_lang$core$Basics$cos = _elm_lang$core$Native_Basics.cos;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['^'] = _elm_lang$core$Native_Basics.exp;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['%'] = _elm_lang$core$Native_Basics.mod;
var _elm_lang$core$Basics$rem = _elm_lang$core$Native_Basics.rem;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['//'] = _elm_lang$core$Native_Basics.div;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['/'] = _elm_lang$core$Native_Basics.floatDiv;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['*'] = _elm_lang$core$Native_Basics.mul;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['-'] = _elm_lang$core$Native_Basics.sub;
var _elm_lang$core$Basics_ops = _elm_lang$core$Basics_ops || {};
_elm_lang$core$Basics_ops['+'] = _elm_lang$core$Native_Basics.add;
var _elm_lang$core$Basics$toPolar = _elm_lang$core$Native_Basics.toPolar;
var _elm_lang$core$Basics$fromPolar = _elm_lang$core$Native_Basics.fromPolar;
var _elm_lang$core$Basics$turns = _elm_lang$core$Native_Basics.turns;
var _elm_lang$core$Basics$degrees = _elm_lang$core$Native_Basics.degrees;
var _elm_lang$core$Basics$radians = function (t) {
	return t;
};
var _elm_lang$core$Basics$GT = {ctor: 'GT'};
var _elm_lang$core$Basics$EQ = {ctor: 'EQ'};
var _elm_lang$core$Basics$LT = {ctor: 'LT'};
var _elm_lang$core$Basics$JustOneMore = function (a) {
	return {ctor: 'JustOneMore', _0: a};
};

var _elm_lang$core$Maybe$withDefault = F2(
	function ($default, maybe) {
		var _p0 = maybe;
		if (_p0.ctor === 'Just') {
			return _p0._0;
		} else {
			return $default;
		}
	});
var _elm_lang$core$Maybe$Nothing = {ctor: 'Nothing'};
var _elm_lang$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		var _p1 = maybeValue;
		if (_p1.ctor === 'Just') {
			return callback(_p1._0);
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$Just = function (a) {
	return {ctor: 'Just', _0: a};
};
var _elm_lang$core$Maybe$map = F2(
	function (f, maybe) {
		var _p2 = maybe;
		if (_p2.ctor === 'Just') {
			return _elm_lang$core$Maybe$Just(
				f(_p2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		var _p3 = {ctor: '_Tuple2', _0: ma, _1: mb};
		if (((_p3.ctor === '_Tuple2') && (_p3._0.ctor === 'Just')) && (_p3._1.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A2(func, _p3._0._0, _p3._1._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map3 = F4(
	function (func, ma, mb, mc) {
		var _p4 = {ctor: '_Tuple3', _0: ma, _1: mb, _2: mc};
		if ((((_p4.ctor === '_Tuple3') && (_p4._0.ctor === 'Just')) && (_p4._1.ctor === 'Just')) && (_p4._2.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A3(func, _p4._0._0, _p4._1._0, _p4._2._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		var _p5 = {ctor: '_Tuple4', _0: ma, _1: mb, _2: mc, _3: md};
		if (((((_p5.ctor === '_Tuple4') && (_p5._0.ctor === 'Just')) && (_p5._1.ctor === 'Just')) && (_p5._2.ctor === 'Just')) && (_p5._3.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A4(func, _p5._0._0, _p5._1._0, _p5._2._0, _p5._3._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});
var _elm_lang$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		var _p6 = {ctor: '_Tuple5', _0: ma, _1: mb, _2: mc, _3: md, _4: me};
		if ((((((_p6.ctor === '_Tuple5') && (_p6._0.ctor === 'Just')) && (_p6._1.ctor === 'Just')) && (_p6._2.ctor === 'Just')) && (_p6._3.ctor === 'Just')) && (_p6._4.ctor === 'Just')) {
			return _elm_lang$core$Maybe$Just(
				A5(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0, _p6._4._0));
		} else {
			return _elm_lang$core$Maybe$Nothing;
		}
	});

//import Native.Utils //

var _elm_lang$core$Native_List = function() {

var Nil = { ctor: '[]' };

function Cons(hd, tl)
{
	return { ctor: '::', _0: hd, _1: tl };
}

function fromArray(arr)
{
	var out = Nil;
	for (var i = arr.length; i--; )
	{
		out = Cons(arr[i], out);
	}
	return out;
}

function toArray(xs)
{
	var out = [];
	while (xs.ctor !== '[]')
	{
		out.push(xs._0);
		xs = xs._1;
	}
	return out;
}

function foldr(f, b, xs)
{
	var arr = toArray(xs);
	var acc = b;
	for (var i = arr.length; i--; )
	{
		acc = A2(f, arr[i], acc);
	}
	return acc;
}

function map2(f, xs, ys)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]')
	{
		arr.push(A2(f, xs._0, ys._0));
		xs = xs._1;
		ys = ys._1;
	}
	return fromArray(arr);
}

function map3(f, xs, ys, zs)
{
	var arr = [];
	while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
	{
		arr.push(A3(f, xs._0, ys._0, zs._0));
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map4(f, ws, xs, ys, zs)
{
	var arr = [];
	while (   ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function map5(f, vs, ws, xs, ys, zs)
{
	var arr = [];
	while (   vs.ctor !== '[]'
		   && ws.ctor !== '[]'
		   && xs.ctor !== '[]'
		   && ys.ctor !== '[]'
		   && zs.ctor !== '[]')
	{
		arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
		vs = vs._1;
		ws = ws._1;
		xs = xs._1;
		ys = ys._1;
		zs = zs._1;
	}
	return fromArray(arr);
}

function sortBy(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		return _elm_lang$core$Native_Utils.cmp(f(a), f(b));
	}));
}

function sortWith(f, xs)
{
	return fromArray(toArray(xs).sort(function(a, b) {
		var ord = f(a)(b).ctor;
		return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
	}));
}

return {
	Nil: Nil,
	Cons: Cons,
	cons: F2(Cons),
	toArray: toArray,
	fromArray: fromArray,

	foldr: F3(foldr),

	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	sortBy: F2(sortBy),
	sortWith: F2(sortWith)
};

}();
var _elm_lang$core$List$sortWith = _elm_lang$core$Native_List.sortWith;
var _elm_lang$core$List$sortBy = _elm_lang$core$Native_List.sortBy;
var _elm_lang$core$List$sort = function (xs) {
	return A2(_elm_lang$core$List$sortBy, _elm_lang$core$Basics$identity, xs);
};
var _elm_lang$core$List$singleton = function (value) {
	return {
		ctor: '::',
		_0: value,
		_1: {ctor: '[]'}
	};
};
var _elm_lang$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return list;
			} else {
				var _p0 = list;
				if (_p0.ctor === '[]') {
					return list;
				} else {
					var _v1 = n - 1,
						_v2 = _p0._1;
					n = _v1;
					list = _v2;
					continue drop;
				}
			}
		}
	});
var _elm_lang$core$List$map5 = _elm_lang$core$Native_List.map5;
var _elm_lang$core$List$map4 = _elm_lang$core$Native_List.map4;
var _elm_lang$core$List$map3 = _elm_lang$core$Native_List.map3;
var _elm_lang$core$List$map2 = _elm_lang$core$Native_List.map2;
var _elm_lang$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			var _p1 = list;
			if (_p1.ctor === '[]') {
				return false;
			} else {
				if (isOkay(_p1._0)) {
					return true;
				} else {
					var _v4 = isOkay,
						_v5 = _p1._1;
					isOkay = _v4;
					list = _v5;
					continue any;
				}
			}
		}
	});
var _elm_lang$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			_elm_lang$core$List$any,
			function (_p2) {
				return !isOkay(_p2);
			},
			list);
	});
var _elm_lang$core$List$foldr = _elm_lang$core$Native_List.foldr;
var _elm_lang$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			var _p3 = list;
			if (_p3.ctor === '[]') {
				return acc;
			} else {
				var _v7 = func,
					_v8 = A2(func, _p3._0, acc),
					_v9 = _p3._1;
				func = _v7;
				acc = _v8;
				list = _v9;
				continue foldl;
			}
		}
	});
var _elm_lang$core$List$length = function (xs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p4, i) {
				return i + 1;
			}),
		0,
		xs);
};
var _elm_lang$core$List$sum = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		numbers);
};
var _elm_lang$core$List$product = function (numbers) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return x * y;
			}),
		1,
		numbers);
};
var _elm_lang$core$List$maximum = function (list) {
	var _p5 = list;
	if (_p5.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$max, _p5._0, _p5._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$minimum = function (list) {
	var _p6 = list;
	if (_p6.ctor === '::') {
		return _elm_lang$core$Maybe$Just(
			A3(_elm_lang$core$List$foldl, _elm_lang$core$Basics$min, _p6._0, _p6._1));
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$member = F2(
	function (x, xs) {
		return A2(
			_elm_lang$core$List$any,
			function (a) {
				return _elm_lang$core$Native_Utils.eq(a, x);
			},
			xs);
	});
var _elm_lang$core$List$isEmpty = function (xs) {
	var _p7 = xs;
	if (_p7.ctor === '[]') {
		return true;
	} else {
		return false;
	}
};
var _elm_lang$core$List$tail = function (list) {
	var _p8 = list;
	if (_p8.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p8._1);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List$head = function (list) {
	var _p9 = list;
	if (_p9.ctor === '::') {
		return _elm_lang$core$Maybe$Just(_p9._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$List_ops = _elm_lang$core$List_ops || {};
_elm_lang$core$List_ops['::'] = _elm_lang$core$Native_List.cons;
var _elm_lang$core$List$map = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			F2(
				function (x, acc) {
					return {
						ctor: '::',
						_0: f(x),
						_1: acc
					};
				}),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$filter = F2(
	function (pred, xs) {
		var conditionalCons = F2(
			function (front, back) {
				return pred(front) ? {ctor: '::', _0: front, _1: back} : back;
			});
		return A3(
			_elm_lang$core$List$foldr,
			conditionalCons,
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _p10 = f(mx);
		if (_p10.ctor === 'Just') {
			return {ctor: '::', _0: _p10._0, _1: xs};
		} else {
			return xs;
		}
	});
var _elm_lang$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$foldr,
			_elm_lang$core$List$maybeCons(f),
			{ctor: '[]'},
			xs);
	});
var _elm_lang$core$List$reverse = function (list) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (x, y) {
				return {ctor: '::', _0: x, _1: y};
			}),
		{ctor: '[]'},
		list);
};
var _elm_lang$core$List$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				var _p11 = accAcc;
				if (_p11.ctor === '::') {
					return {
						ctor: '::',
						_0: A2(f, x, _p11._0),
						_1: accAcc
					};
				} else {
					return {ctor: '[]'};
				}
			});
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$foldl,
				scan1,
				{
					ctor: '::',
					_0: b,
					_1: {ctor: '[]'}
				},
				xs));
	});
var _elm_lang$core$List$append = F2(
	function (xs, ys) {
		var _p12 = ys;
		if (_p12.ctor === '[]') {
			return xs;
		} else {
			return A3(
				_elm_lang$core$List$foldr,
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				ys,
				xs);
		}
	});
var _elm_lang$core$List$concat = function (lists) {
	return A3(
		_elm_lang$core$List$foldr,
		_elm_lang$core$List$append,
		{ctor: '[]'},
		lists);
};
var _elm_lang$core$List$concatMap = F2(
	function (f, list) {
		return _elm_lang$core$List$concat(
			A2(_elm_lang$core$List$map, f, list));
	});
var _elm_lang$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _p13) {
				var _p14 = _p13;
				var _p16 = _p14._0;
				var _p15 = _p14._1;
				return pred(x) ? {
					ctor: '_Tuple2',
					_0: {ctor: '::', _0: x, _1: _p16},
					_1: _p15
				} : {
					ctor: '_Tuple2',
					_0: _p16,
					_1: {ctor: '::', _0: x, _1: _p15}
				};
			});
		return A3(
			_elm_lang$core$List$foldr,
			step,
			{
				ctor: '_Tuple2',
				_0: {ctor: '[]'},
				_1: {ctor: '[]'}
			},
			list);
	});
var _elm_lang$core$List$unzip = function (pairs) {
	var step = F2(
		function (_p18, _p17) {
			var _p19 = _p18;
			var _p20 = _p17;
			return {
				ctor: '_Tuple2',
				_0: {ctor: '::', _0: _p19._0, _1: _p20._0},
				_1: {ctor: '::', _0: _p19._1, _1: _p20._1}
			};
		});
	return A3(
		_elm_lang$core$List$foldr,
		step,
		{
			ctor: '_Tuple2',
			_0: {ctor: '[]'},
			_1: {ctor: '[]'}
		},
		pairs);
};
var _elm_lang$core$List$intersperse = F2(
	function (sep, xs) {
		var _p21 = xs;
		if (_p21.ctor === '[]') {
			return {ctor: '[]'};
		} else {
			var step = F2(
				function (x, rest) {
					return {
						ctor: '::',
						_0: sep,
						_1: {ctor: '::', _0: x, _1: rest}
					};
				});
			var spersed = A3(
				_elm_lang$core$List$foldr,
				step,
				{ctor: '[]'},
				_p21._1);
			return {ctor: '::', _0: _p21._0, _1: spersed};
		}
	});
var _elm_lang$core$List$takeReverse = F3(
	function (n, list, taken) {
		takeReverse:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return taken;
			} else {
				var _p22 = list;
				if (_p22.ctor === '[]') {
					return taken;
				} else {
					var _v23 = n - 1,
						_v24 = _p22._1,
						_v25 = {ctor: '::', _0: _p22._0, _1: taken};
					n = _v23;
					list = _v24;
					taken = _v25;
					continue takeReverse;
				}
			}
		}
	});
var _elm_lang$core$List$takeTailRec = F2(
	function (n, list) {
		return _elm_lang$core$List$reverse(
			A3(
				_elm_lang$core$List$takeReverse,
				n,
				list,
				{ctor: '[]'}));
	});
var _elm_lang$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
			return {ctor: '[]'};
		} else {
			var _p23 = {ctor: '_Tuple2', _0: n, _1: list};
			_v26_5:
			do {
				_v26_1:
				do {
					if (_p23.ctor === '_Tuple2') {
						if (_p23._1.ctor === '[]') {
							return list;
						} else {
							if (_p23._1._1.ctor === '::') {
								switch (_p23._0) {
									case 1:
										break _v26_1;
									case 2:
										return {
											ctor: '::',
											_0: _p23._1._0,
											_1: {
												ctor: '::',
												_0: _p23._1._1._0,
												_1: {ctor: '[]'}
											}
										};
									case 3:
										if (_p23._1._1._1.ctor === '::') {
											return {
												ctor: '::',
												_0: _p23._1._0,
												_1: {
													ctor: '::',
													_0: _p23._1._1._0,
													_1: {
														ctor: '::',
														_0: _p23._1._1._1._0,
														_1: {ctor: '[]'}
													}
												}
											};
										} else {
											break _v26_5;
										}
									default:
										if ((_p23._1._1._1.ctor === '::') && (_p23._1._1._1._1.ctor === '::')) {
											var _p28 = _p23._1._1._1._0;
											var _p27 = _p23._1._1._0;
											var _p26 = _p23._1._0;
											var _p25 = _p23._1._1._1._1._0;
											var _p24 = _p23._1._1._1._1._1;
											return (_elm_lang$core$Native_Utils.cmp(ctr, 1000) > 0) ? {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A2(_elm_lang$core$List$takeTailRec, n - 4, _p24)
														}
													}
												}
											} : {
												ctor: '::',
												_0: _p26,
												_1: {
													ctor: '::',
													_0: _p27,
													_1: {
														ctor: '::',
														_0: _p28,
														_1: {
															ctor: '::',
															_0: _p25,
															_1: A3(_elm_lang$core$List$takeFast, ctr + 1, n - 4, _p24)
														}
													}
												}
											};
										} else {
											break _v26_5;
										}
								}
							} else {
								if (_p23._0 === 1) {
									break _v26_1;
								} else {
									break _v26_5;
								}
							}
						}
					} else {
						break _v26_5;
					}
				} while(false);
				return {
					ctor: '::',
					_0: _p23._1._0,
					_1: {ctor: '[]'}
				};
			} while(false);
			return list;
		}
	});
var _elm_lang$core$List$take = F2(
	function (n, list) {
		return A3(_elm_lang$core$List$takeFast, 0, n, list);
	});
var _elm_lang$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 0) < 1) {
				return result;
			} else {
				var _v27 = {ctor: '::', _0: value, _1: result},
					_v28 = n - 1,
					_v29 = value;
				result = _v27;
				n = _v28;
				value = _v29;
				continue repeatHelp;
			}
		}
	});
var _elm_lang$core$List$repeat = F2(
	function (n, value) {
		return A3(
			_elm_lang$core$List$repeatHelp,
			{ctor: '[]'},
			n,
			value);
	});
var _elm_lang$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(lo, hi) < 1) {
				var _v30 = lo,
					_v31 = hi - 1,
					_v32 = {ctor: '::', _0: hi, _1: list};
				lo = _v30;
				hi = _v31;
				list = _v32;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var _elm_lang$core$List$range = F2(
	function (lo, hi) {
		return A3(
			_elm_lang$core$List$rangeHelp,
			lo,
			hi,
			{ctor: '[]'});
	});
var _elm_lang$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			_elm_lang$core$List$map2,
			f,
			A2(
				_elm_lang$core$List$range,
				0,
				_elm_lang$core$List$length(xs) - 1),
			xs);
	});

var _elm_lang$core$Array$append = _elm_lang$core$Native_Array.append;
var _elm_lang$core$Array$length = _elm_lang$core$Native_Array.length;
var _elm_lang$core$Array$isEmpty = function (array) {
	return _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(array),
		0);
};
var _elm_lang$core$Array$slice = _elm_lang$core$Native_Array.slice;
var _elm_lang$core$Array$set = _elm_lang$core$Native_Array.set;
var _elm_lang$core$Array$get = F2(
	function (i, array) {
		return ((_elm_lang$core$Native_Utils.cmp(0, i) < 1) && (_elm_lang$core$Native_Utils.cmp(
			i,
			_elm_lang$core$Native_Array.length(array)) < 0)) ? _elm_lang$core$Maybe$Just(
			A2(_elm_lang$core$Native_Array.get, i, array)) : _elm_lang$core$Maybe$Nothing;
	});
var _elm_lang$core$Array$push = _elm_lang$core$Native_Array.push;
var _elm_lang$core$Array$empty = _elm_lang$core$Native_Array.empty;
var _elm_lang$core$Array$filter = F2(
	function (isOkay, arr) {
		var update = F2(
			function (x, xs) {
				return isOkay(x) ? A2(_elm_lang$core$Native_Array.push, x, xs) : xs;
			});
		return A3(_elm_lang$core$Native_Array.foldl, update, _elm_lang$core$Native_Array.empty, arr);
	});
var _elm_lang$core$Array$foldr = _elm_lang$core$Native_Array.foldr;
var _elm_lang$core$Array$foldl = _elm_lang$core$Native_Array.foldl;
var _elm_lang$core$Array$indexedMap = _elm_lang$core$Native_Array.indexedMap;
var _elm_lang$core$Array$map = _elm_lang$core$Native_Array.map;
var _elm_lang$core$Array$toIndexedList = function (array) {
	return A3(
		_elm_lang$core$List$map2,
		F2(
			function (v0, v1) {
				return {ctor: '_Tuple2', _0: v0, _1: v1};
			}),
		A2(
			_elm_lang$core$List$range,
			0,
			_elm_lang$core$Native_Array.length(array) - 1),
		_elm_lang$core$Native_Array.toList(array));
};
var _elm_lang$core$Array$toList = _elm_lang$core$Native_Array.toList;
var _elm_lang$core$Array$fromList = _elm_lang$core$Native_Array.fromList;
var _elm_lang$core$Array$initialize = _elm_lang$core$Native_Array.initialize;
var _elm_lang$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			_elm_lang$core$Array$initialize,
			n,
			_elm_lang$core$Basics$always(e));
	});
var _elm_lang$core$Array$Array = {ctor: 'Array'};

//import Native.Utils //

var _elm_lang$core$Native_Debug = function() {

function log(tag, value)
{
	var msg = tag + ': ' + _elm_lang$core$Native_Utils.toString(value);
	var process = process || {};
	if (process.stdout)
	{
		process.stdout.write(msg);
	}
	else
	{
		console.log(msg);
	}
	return value;
}

function crash(message)
{
	throw new Error(message);
}

return {
	crash: crash,
	log: F2(log)
};

}();
//import Maybe, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_String = function() {

function isEmpty(str)
{
	return str.length === 0;
}
function cons(chr, str)
{
	return chr + str;
}
function uncons(str)
{
	var hd = str[0];
	if (hd)
	{
		return _elm_lang$core$Maybe$Just(_elm_lang$core$Native_Utils.Tuple2(_elm_lang$core$Native_Utils.chr(hd), str.slice(1)));
	}
	return _elm_lang$core$Maybe$Nothing;
}
function append(a, b)
{
	return a + b;
}
function concat(strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join('');
}
function length(str)
{
	return str.length;
}
function map(f, str)
{
	var out = str.split('');
	for (var i = out.length; i--; )
	{
		out[i] = f(_elm_lang$core$Native_Utils.chr(out[i]));
	}
	return out.join('');
}
function filter(pred, str)
{
	return str.split('').map(_elm_lang$core$Native_Utils.chr).filter(pred).join('');
}
function reverse(str)
{
	return str.split('').reverse().join('');
}
function foldl(f, b, str)
{
	var len = str.length;
	for (var i = 0; i < len; ++i)
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function foldr(f, b, str)
{
	for (var i = str.length; i--; )
	{
		b = A2(f, _elm_lang$core$Native_Utils.chr(str[i]), b);
	}
	return b;
}
function split(sep, str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(sep));
}
function join(sep, strs)
{
	return _elm_lang$core$Native_List.toArray(strs).join(sep);
}
function repeat(n, str)
{
	var result = '';
	while (n > 0)
	{
		if (n & 1)
		{
			result += str;
		}
		n >>= 1, str += str;
	}
	return result;
}
function slice(start, end, str)
{
	return str.slice(start, end);
}
function left(n, str)
{
	return n < 1 ? '' : str.slice(0, n);
}
function right(n, str)
{
	return n < 1 ? '' : str.slice(-n);
}
function dropLeft(n, str)
{
	return n < 1 ? str : str.slice(n);
}
function dropRight(n, str)
{
	return n < 1 ? str : str.slice(0, -n);
}
function pad(n, chr, str)
{
	var half = (n - str.length) / 2;
	return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
}
function padRight(n, chr, str)
{
	return str + repeat(n - str.length, chr);
}
function padLeft(n, chr, str)
{
	return repeat(n - str.length, chr) + str;
}

function trim(str)
{
	return str.trim();
}
function trimLeft(str)
{
	return str.replace(/^\s+/, '');
}
function trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function words(str)
{
	return _elm_lang$core$Native_List.fromArray(str.trim().split(/\s+/g));
}
function lines(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split(/\r\n|\r|\n/g));
}

function toUpper(str)
{
	return str.toUpperCase();
}
function toLower(str)
{
	return str.toLowerCase();
}

function any(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return true;
		}
	}
	return false;
}
function all(pred, str)
{
	for (var i = str.length; i--; )
	{
		if (!pred(_elm_lang$core$Native_Utils.chr(str[i])))
		{
			return false;
		}
	}
	return true;
}

function contains(sub, str)
{
	return str.indexOf(sub) > -1;
}
function startsWith(sub, str)
{
	return str.indexOf(sub) === 0;
}
function endsWith(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
}
function indexes(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _elm_lang$core$Native_List.Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _elm_lang$core$Native_List.fromArray(is);
}


function toInt(s)
{
	var len = s.length;

	// if empty
	if (len === 0)
	{
		return intErr(s);
	}

	// if hex
	var c = s[0];
	if (c === '0' && s[1] === 'x')
	{
		for (var i = 2; i < len; ++i)
		{
			var c = s[i];
			if (('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f'))
			{
				continue;
			}
			return intErr(s);
		}
		return _elm_lang$core$Result$Ok(parseInt(s, 16));
	}

	// is decimal
	if (c > '9' || (c < '0' && c !== '-' && c !== '+'))
	{
		return intErr(s);
	}
	for (var i = 1; i < len; ++i)
	{
		var c = s[i];
		if (c < '0' || '9' < c)
		{
			return intErr(s);
		}
	}

	return _elm_lang$core$Result$Ok(parseInt(s, 10));
}

function intErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to an Int");
}


function toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return floatErr(s);
	}
	var n = +s;
	// faster isNaN check
	return n === n ? _elm_lang$core$Result$Ok(n) : floatErr(s);
}

function floatErr(s)
{
	return _elm_lang$core$Result$Err("could not convert string '" + s + "' to a Float");
}


function toList(str)
{
	return _elm_lang$core$Native_List.fromArray(str.split('').map(_elm_lang$core$Native_Utils.chr));
}
function fromList(chars)
{
	return _elm_lang$core$Native_List.toArray(chars).join('');
}

return {
	isEmpty: isEmpty,
	cons: F2(cons),
	uncons: uncons,
	append: F2(append),
	concat: concat,
	length: length,
	map: F2(map),
	filter: F2(filter),
	reverse: reverse,
	foldl: F3(foldl),
	foldr: F3(foldr),

	split: F2(split),
	join: F2(join),
	repeat: F2(repeat),

	slice: F3(slice),
	left: F2(left),
	right: F2(right),
	dropLeft: F2(dropLeft),
	dropRight: F2(dropRight),

	pad: F3(pad),
	padLeft: F3(padLeft),
	padRight: F3(padRight),

	trim: trim,
	trimLeft: trimLeft,
	trimRight: trimRight,

	words: words,
	lines: lines,

	toUpper: toUpper,
	toLower: toLower,

	any: F2(any),
	all: F2(all),

	contains: F2(contains),
	startsWith: F2(startsWith),
	endsWith: F2(endsWith),
	indexes: F2(indexes),

	toInt: toInt,
	toFloat: toFloat,
	toList: toList,
	fromList: fromList
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Char = function() {

return {
	fromCode: function(c) { return _elm_lang$core$Native_Utils.chr(String.fromCharCode(c)); },
	toCode: function(c) { return c.charCodeAt(0); },
	toUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toUpperCase()); },
	toLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLowerCase()); },
	toLocaleUpper: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleUpperCase()); },
	toLocaleLower: function(c) { return _elm_lang$core$Native_Utils.chr(c.toLocaleLowerCase()); }
};

}();
var _elm_lang$core$Char$fromCode = _elm_lang$core$Native_Char.fromCode;
var _elm_lang$core$Char$toCode = _elm_lang$core$Native_Char.toCode;
var _elm_lang$core$Char$toLocaleLower = _elm_lang$core$Native_Char.toLocaleLower;
var _elm_lang$core$Char$toLocaleUpper = _elm_lang$core$Native_Char.toLocaleUpper;
var _elm_lang$core$Char$toLower = _elm_lang$core$Native_Char.toLower;
var _elm_lang$core$Char$toUpper = _elm_lang$core$Native_Char.toUpper;
var _elm_lang$core$Char$isBetween = F3(
	function (low, high, $char) {
		var code = _elm_lang$core$Char$toCode($char);
		return (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(low)) > -1) && (_elm_lang$core$Native_Utils.cmp(
			code,
			_elm_lang$core$Char$toCode(high)) < 1);
	});
var _elm_lang$core$Char$isUpper = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('A'),
	_elm_lang$core$Native_Utils.chr('Z'));
var _elm_lang$core$Char$isLower = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('a'),
	_elm_lang$core$Native_Utils.chr('z'));
var _elm_lang$core$Char$isDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('9'));
var _elm_lang$core$Char$isOctDigit = A2(
	_elm_lang$core$Char$isBetween,
	_elm_lang$core$Native_Utils.chr('0'),
	_elm_lang$core$Native_Utils.chr('7'));
var _elm_lang$core$Char$isHexDigit = function ($char) {
	return _elm_lang$core$Char$isDigit($char) || (A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('a'),
		_elm_lang$core$Native_Utils.chr('f'),
		$char) || A3(
		_elm_lang$core$Char$isBetween,
		_elm_lang$core$Native_Utils.chr('A'),
		_elm_lang$core$Native_Utils.chr('F'),
		$char));
};

var _elm_lang$core$Result$toMaybe = function (result) {
	var _p0 = result;
	if (_p0.ctor === 'Ok') {
		return _elm_lang$core$Maybe$Just(_p0._0);
	} else {
		return _elm_lang$core$Maybe$Nothing;
	}
};
var _elm_lang$core$Result$withDefault = F2(
	function (def, result) {
		var _p1 = result;
		if (_p1.ctor === 'Ok') {
			return _p1._0;
		} else {
			return def;
		}
	});
var _elm_lang$core$Result$Err = function (a) {
	return {ctor: 'Err', _0: a};
};
var _elm_lang$core$Result$andThen = F2(
	function (callback, result) {
		var _p2 = result;
		if (_p2.ctor === 'Ok') {
			return callback(_p2._0);
		} else {
			return _elm_lang$core$Result$Err(_p2._0);
		}
	});
var _elm_lang$core$Result$Ok = function (a) {
	return {ctor: 'Ok', _0: a};
};
var _elm_lang$core$Result$map = F2(
	function (func, ra) {
		var _p3 = ra;
		if (_p3.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(
				func(_p3._0));
		} else {
			return _elm_lang$core$Result$Err(_p3._0);
		}
	});
var _elm_lang$core$Result$map2 = F3(
	function (func, ra, rb) {
		var _p4 = {ctor: '_Tuple2', _0: ra, _1: rb};
		if (_p4._0.ctor === 'Ok') {
			if (_p4._1.ctor === 'Ok') {
				return _elm_lang$core$Result$Ok(
					A2(func, _p4._0._0, _p4._1._0));
			} else {
				return _elm_lang$core$Result$Err(_p4._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p4._0._0);
		}
	});
var _elm_lang$core$Result$map3 = F4(
	function (func, ra, rb, rc) {
		var _p5 = {ctor: '_Tuple3', _0: ra, _1: rb, _2: rc};
		if (_p5._0.ctor === 'Ok') {
			if (_p5._1.ctor === 'Ok') {
				if (_p5._2.ctor === 'Ok') {
					return _elm_lang$core$Result$Ok(
						A3(func, _p5._0._0, _p5._1._0, _p5._2._0));
				} else {
					return _elm_lang$core$Result$Err(_p5._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p5._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p5._0._0);
		}
	});
var _elm_lang$core$Result$map4 = F5(
	function (func, ra, rb, rc, rd) {
		var _p6 = {ctor: '_Tuple4', _0: ra, _1: rb, _2: rc, _3: rd};
		if (_p6._0.ctor === 'Ok') {
			if (_p6._1.ctor === 'Ok') {
				if (_p6._2.ctor === 'Ok') {
					if (_p6._3.ctor === 'Ok') {
						return _elm_lang$core$Result$Ok(
							A4(func, _p6._0._0, _p6._1._0, _p6._2._0, _p6._3._0));
					} else {
						return _elm_lang$core$Result$Err(_p6._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p6._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p6._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p6._0._0);
		}
	});
var _elm_lang$core$Result$map5 = F6(
	function (func, ra, rb, rc, rd, re) {
		var _p7 = {ctor: '_Tuple5', _0: ra, _1: rb, _2: rc, _3: rd, _4: re};
		if (_p7._0.ctor === 'Ok') {
			if (_p7._1.ctor === 'Ok') {
				if (_p7._2.ctor === 'Ok') {
					if (_p7._3.ctor === 'Ok') {
						if (_p7._4.ctor === 'Ok') {
							return _elm_lang$core$Result$Ok(
								A5(func, _p7._0._0, _p7._1._0, _p7._2._0, _p7._3._0, _p7._4._0));
						} else {
							return _elm_lang$core$Result$Err(_p7._4._0);
						}
					} else {
						return _elm_lang$core$Result$Err(_p7._3._0);
					}
				} else {
					return _elm_lang$core$Result$Err(_p7._2._0);
				}
			} else {
				return _elm_lang$core$Result$Err(_p7._1._0);
			}
		} else {
			return _elm_lang$core$Result$Err(_p7._0._0);
		}
	});
var _elm_lang$core$Result$mapError = F2(
	function (f, result) {
		var _p8 = result;
		if (_p8.ctor === 'Ok') {
			return _elm_lang$core$Result$Ok(_p8._0);
		} else {
			return _elm_lang$core$Result$Err(
				f(_p8._0));
		}
	});
var _elm_lang$core$Result$fromMaybe = F2(
	function (err, maybe) {
		var _p9 = maybe;
		if (_p9.ctor === 'Just') {
			return _elm_lang$core$Result$Ok(_p9._0);
		} else {
			return _elm_lang$core$Result$Err(err);
		}
	});

var _elm_lang$core$String$fromList = _elm_lang$core$Native_String.fromList;
var _elm_lang$core$String$toList = _elm_lang$core$Native_String.toList;
var _elm_lang$core$String$toFloat = _elm_lang$core$Native_String.toFloat;
var _elm_lang$core$String$toInt = _elm_lang$core$Native_String.toInt;
var _elm_lang$core$String$indices = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$indexes = _elm_lang$core$Native_String.indexes;
var _elm_lang$core$String$endsWith = _elm_lang$core$Native_String.endsWith;
var _elm_lang$core$String$startsWith = _elm_lang$core$Native_String.startsWith;
var _elm_lang$core$String$contains = _elm_lang$core$Native_String.contains;
var _elm_lang$core$String$all = _elm_lang$core$Native_String.all;
var _elm_lang$core$String$any = _elm_lang$core$Native_String.any;
var _elm_lang$core$String$toLower = _elm_lang$core$Native_String.toLower;
var _elm_lang$core$String$toUpper = _elm_lang$core$Native_String.toUpper;
var _elm_lang$core$String$lines = _elm_lang$core$Native_String.lines;
var _elm_lang$core$String$words = _elm_lang$core$Native_String.words;
var _elm_lang$core$String$trimRight = _elm_lang$core$Native_String.trimRight;
var _elm_lang$core$String$trimLeft = _elm_lang$core$Native_String.trimLeft;
var _elm_lang$core$String$trim = _elm_lang$core$Native_String.trim;
var _elm_lang$core$String$padRight = _elm_lang$core$Native_String.padRight;
var _elm_lang$core$String$padLeft = _elm_lang$core$Native_String.padLeft;
var _elm_lang$core$String$pad = _elm_lang$core$Native_String.pad;
var _elm_lang$core$String$dropRight = _elm_lang$core$Native_String.dropRight;
var _elm_lang$core$String$dropLeft = _elm_lang$core$Native_String.dropLeft;
var _elm_lang$core$String$right = _elm_lang$core$Native_String.right;
var _elm_lang$core$String$left = _elm_lang$core$Native_String.left;
var _elm_lang$core$String$slice = _elm_lang$core$Native_String.slice;
var _elm_lang$core$String$repeat = _elm_lang$core$Native_String.repeat;
var _elm_lang$core$String$join = _elm_lang$core$Native_String.join;
var _elm_lang$core$String$split = _elm_lang$core$Native_String.split;
var _elm_lang$core$String$foldr = _elm_lang$core$Native_String.foldr;
var _elm_lang$core$String$foldl = _elm_lang$core$Native_String.foldl;
var _elm_lang$core$String$reverse = _elm_lang$core$Native_String.reverse;
var _elm_lang$core$String$filter = _elm_lang$core$Native_String.filter;
var _elm_lang$core$String$map = _elm_lang$core$Native_String.map;
var _elm_lang$core$String$length = _elm_lang$core$Native_String.length;
var _elm_lang$core$String$concat = _elm_lang$core$Native_String.concat;
var _elm_lang$core$String$append = _elm_lang$core$Native_String.append;
var _elm_lang$core$String$uncons = _elm_lang$core$Native_String.uncons;
var _elm_lang$core$String$cons = _elm_lang$core$Native_String.cons;
var _elm_lang$core$String$fromChar = function ($char) {
	return A2(_elm_lang$core$String$cons, $char, '');
};
var _elm_lang$core$String$isEmpty = _elm_lang$core$Native_String.isEmpty;

var _elm_lang$core$Dict$foldr = F3(
	function (f, acc, t) {
		foldr:
		while (true) {
			var _p0 = t;
			if (_p0.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v1 = f,
					_v2 = A3(
					f,
					_p0._1,
					_p0._2,
					A3(_elm_lang$core$Dict$foldr, f, acc, _p0._4)),
					_v3 = _p0._3;
				f = _v1;
				acc = _v2;
				t = _v3;
				continue foldr;
			}
		}
	});
var _elm_lang$core$Dict$keys = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return {ctor: '::', _0: key, _1: keyList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$values = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return {ctor: '::', _0: value, _1: valueList};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$toList = function (dict) {
	return A3(
		_elm_lang$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: key, _1: value},
					_1: list
				};
			}),
		{ctor: '[]'},
		dict);
};
var _elm_lang$core$Dict$foldl = F3(
	function (f, acc, dict) {
		foldl:
		while (true) {
			var _p1 = dict;
			if (_p1.ctor === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var _v5 = f,
					_v6 = A3(
					f,
					_p1._1,
					_p1._2,
					A3(_elm_lang$core$Dict$foldl, f, acc, _p1._3)),
					_v7 = _p1._4;
				f = _v5;
				acc = _v6;
				dict = _v7;
				continue foldl;
			}
		}
	});
var _elm_lang$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _p2) {
				stepState:
				while (true) {
					var _p3 = _p2;
					var _p9 = _p3._1;
					var _p8 = _p3._0;
					var _p4 = _p8;
					if (_p4.ctor === '[]') {
						return {
							ctor: '_Tuple2',
							_0: _p8,
							_1: A3(rightStep, rKey, rValue, _p9)
						};
					} else {
						var _p7 = _p4._1;
						var _p6 = _p4._0._1;
						var _p5 = _p4._0._0;
						if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) < 0) {
							var _v10 = rKey,
								_v11 = rValue,
								_v12 = {
								ctor: '_Tuple2',
								_0: _p7,
								_1: A3(leftStep, _p5, _p6, _p9)
							};
							rKey = _v10;
							rValue = _v11;
							_p2 = _v12;
							continue stepState;
						} else {
							if (_elm_lang$core$Native_Utils.cmp(_p5, rKey) > 0) {
								return {
									ctor: '_Tuple2',
									_0: _p8,
									_1: A3(rightStep, rKey, rValue, _p9)
								};
							} else {
								return {
									ctor: '_Tuple2',
									_0: _p7,
									_1: A4(bothStep, _p5, _p6, rValue, _p9)
								};
							}
						}
					}
				}
			});
		var _p10 = A3(
			_elm_lang$core$Dict$foldl,
			stepState,
			{
				ctor: '_Tuple2',
				_0: _elm_lang$core$Dict$toList(leftDict),
				_1: initialResult
			},
			rightDict);
		var leftovers = _p10._0;
		var intermediateResult = _p10._1;
		return A3(
			_elm_lang$core$List$foldl,
			F2(
				function (_p11, result) {
					var _p12 = _p11;
					return A3(leftStep, _p12._0, _p12._1, result);
				}),
			intermediateResult,
			leftovers);
	});
var _elm_lang$core$Dict$reportRemBug = F4(
	function (msg, c, lgot, rgot) {
		return _elm_lang$core$Native_Debug.crash(
			_elm_lang$core$String$concat(
				{
					ctor: '::',
					_0: 'Internal red-black tree invariant violated, expected ',
					_1: {
						ctor: '::',
						_0: msg,
						_1: {
							ctor: '::',
							_0: ' and got ',
							_1: {
								ctor: '::',
								_0: _elm_lang$core$Basics$toString(c),
								_1: {
									ctor: '::',
									_0: '/',
									_1: {
										ctor: '::',
										_0: lgot,
										_1: {
											ctor: '::',
											_0: '/',
											_1: {
												ctor: '::',
												_0: rgot,
												_1: {
													ctor: '::',
													_0: '\nPlease report this bug to <https://github.com/elm-lang/core/issues>',
													_1: {ctor: '[]'}
												}
											}
										}
									}
								}
							}
						}
					}
				}));
	});
var _elm_lang$core$Dict$isBBlack = function (dict) {
	var _p13 = dict;
	_v14_2:
	do {
		if (_p13.ctor === 'RBNode_elm_builtin') {
			if (_p13._0.ctor === 'BBlack') {
				return true;
			} else {
				break _v14_2;
			}
		} else {
			if (_p13._0.ctor === 'LBBlack') {
				return true;
			} else {
				break _v14_2;
			}
		}
	} while(false);
	return false;
};
var _elm_lang$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			var _p14 = dict;
			if (_p14.ctor === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var _v16 = A2(_elm_lang$core$Dict$sizeHelp, n + 1, _p14._4),
					_v17 = _p14._3;
				n = _v16;
				dict = _v17;
				continue sizeHelp;
			}
		}
	});
var _elm_lang$core$Dict$size = function (dict) {
	return A2(_elm_lang$core$Dict$sizeHelp, 0, dict);
};
var _elm_lang$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			var _p15 = dict;
			if (_p15.ctor === 'RBEmpty_elm_builtin') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p16 = A2(_elm_lang$core$Basics$compare, targetKey, _p15._1);
				switch (_p16.ctor) {
					case 'LT':
						var _v20 = targetKey,
							_v21 = _p15._3;
						targetKey = _v20;
						dict = _v21;
						continue get;
					case 'EQ':
						return _elm_lang$core$Maybe$Just(_p15._2);
					default:
						var _v22 = targetKey,
							_v23 = _p15._4;
						targetKey = _v22;
						dict = _v23;
						continue get;
				}
			}
		}
	});
var _elm_lang$core$Dict$member = F2(
	function (key, dict) {
		var _p17 = A2(_elm_lang$core$Dict$get, key, dict);
		if (_p17.ctor === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var _elm_lang$core$Dict$maxWithDefault = F3(
	function (k, v, r) {
		maxWithDefault:
		while (true) {
			var _p18 = r;
			if (_p18.ctor === 'RBEmpty_elm_builtin') {
				return {ctor: '_Tuple2', _0: k, _1: v};
			} else {
				var _v26 = _p18._1,
					_v27 = _p18._2,
					_v28 = _p18._4;
				k = _v26;
				v = _v27;
				r = _v28;
				continue maxWithDefault;
			}
		}
	});
var _elm_lang$core$Dict$NBlack = {ctor: 'NBlack'};
var _elm_lang$core$Dict$BBlack = {ctor: 'BBlack'};
var _elm_lang$core$Dict$Black = {ctor: 'Black'};
var _elm_lang$core$Dict$blackish = function (t) {
	var _p19 = t;
	if (_p19.ctor === 'RBNode_elm_builtin') {
		var _p20 = _p19._0;
		return _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$Black) || _elm_lang$core$Native_Utils.eq(_p20, _elm_lang$core$Dict$BBlack);
	} else {
		return true;
	}
};
var _elm_lang$core$Dict$Red = {ctor: 'Red'};
var _elm_lang$core$Dict$moreBlack = function (color) {
	var _p21 = color;
	switch (_p21.ctor) {
		case 'Black':
			return _elm_lang$core$Dict$BBlack;
		case 'Red':
			return _elm_lang$core$Dict$Black;
		case 'NBlack':
			return _elm_lang$core$Dict$Red;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a double black node more black!');
	}
};
var _elm_lang$core$Dict$lessBlack = function (color) {
	var _p22 = color;
	switch (_p22.ctor) {
		case 'BBlack':
			return _elm_lang$core$Dict$Black;
		case 'Black':
			return _elm_lang$core$Dict$Red;
		case 'Red':
			return _elm_lang$core$Dict$NBlack;
		default:
			return _elm_lang$core$Native_Debug.crash('Can\'t make a negative black node less black!');
	}
};
var _elm_lang$core$Dict$LBBlack = {ctor: 'LBBlack'};
var _elm_lang$core$Dict$LBlack = {ctor: 'LBlack'};
var _elm_lang$core$Dict$RBEmpty_elm_builtin = function (a) {
	return {ctor: 'RBEmpty_elm_builtin', _0: a};
};
var _elm_lang$core$Dict$empty = _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
var _elm_lang$core$Dict$isEmpty = function (dict) {
	return _elm_lang$core$Native_Utils.eq(dict, _elm_lang$core$Dict$empty);
};
var _elm_lang$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {ctor: 'RBNode_elm_builtin', _0: a, _1: b, _2: c, _3: d, _4: e};
	});
var _elm_lang$core$Dict$ensureBlackRoot = function (dict) {
	var _p23 = dict;
	if ((_p23.ctor === 'RBNode_elm_builtin') && (_p23._0.ctor === 'Red')) {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p23._1, _p23._2, _p23._3, _p23._4);
	} else {
		return dict;
	}
};
var _elm_lang$core$Dict$lessBlackTree = function (dict) {
	var _p24 = dict;
	if (_p24.ctor === 'RBNode_elm_builtin') {
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$lessBlack(_p24._0),
			_p24._1,
			_p24._2,
			_p24._3,
			_p24._4);
	} else {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	}
};
var _elm_lang$core$Dict$balancedTree = function (col) {
	return function (xk) {
		return function (xv) {
			return function (yk) {
				return function (yv) {
					return function (zk) {
						return function (zv) {
							return function (a) {
								return function (b) {
									return function (c) {
										return function (d) {
											return A5(
												_elm_lang$core$Dict$RBNode_elm_builtin,
												_elm_lang$core$Dict$lessBlack(col),
												yk,
												yv,
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, xk, xv, a, b),
												A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, zk, zv, c, d));
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _elm_lang$core$Dict$blacken = function (t) {
	var _p25 = t;
	if (_p25.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p25._1, _p25._2, _p25._3, _p25._4);
	}
};
var _elm_lang$core$Dict$redden = function (t) {
	var _p26 = t;
	if (_p26.ctor === 'RBEmpty_elm_builtin') {
		return _elm_lang$core$Native_Debug.crash('can\'t make a Leaf red');
	} else {
		return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, _p26._1, _p26._2, _p26._3, _p26._4);
	}
};
var _elm_lang$core$Dict$balanceHelp = function (tree) {
	var _p27 = tree;
	_v36_6:
	do {
		_v36_5:
		do {
			_v36_4:
			do {
				_v36_3:
				do {
					_v36_2:
					do {
						_v36_1:
						do {
							_v36_0:
							do {
								if (_p27.ctor === 'RBNode_elm_builtin') {
									if (_p27._3.ctor === 'RBNode_elm_builtin') {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._3._0.ctor) {
												case 'Red':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																		break _v36_2;
																	} else {
																		if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																			break _v36_3;
																		} else {
																			break _v36_6;
																		}
																	}
																}
															}
														case 'NBlack':
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																		break _v36_4;
																	} else {
																		break _v36_6;
																	}
																}
															}
														default:
															if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
																break _v36_0;
															} else {
																if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
																	break _v36_1;
																} else {
																	break _v36_6;
																}
															}
													}
												case 'NBlack':
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															}
														case 'NBlack':
															if (_p27._0.ctor === 'BBlack') {
																if ((((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																	break _v36_4;
																} else {
																	if ((((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																		break _v36_5;
																	} else {
																		break _v36_6;
																	}
																}
															} else {
																break _v36_6;
															}
														default:
															if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
																break _v36_5;
															} else {
																break _v36_6;
															}
													}
												default:
													switch (_p27._4._0.ctor) {
														case 'Red':
															if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
																break _v36_2;
															} else {
																if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
																	break _v36_3;
																} else {
																	break _v36_6;
																}
															}
														case 'NBlack':
															if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
																break _v36_4;
															} else {
																break _v36_6;
															}
														default:
															break _v36_6;
													}
											}
										} else {
											switch (_p27._3._0.ctor) {
												case 'Red':
													if ((_p27._3._3.ctor === 'RBNode_elm_builtin') && (_p27._3._3._0.ctor === 'Red')) {
														break _v36_0;
													} else {
														if ((_p27._3._4.ctor === 'RBNode_elm_builtin') && (_p27._3._4._0.ctor === 'Red')) {
															break _v36_1;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._3._3.ctor === 'RBNode_elm_builtin')) && (_p27._3._3._0.ctor === 'Black')) && (_p27._3._4.ctor === 'RBNode_elm_builtin')) && (_p27._3._4._0.ctor === 'Black')) {
														break _v36_5;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										}
									} else {
										if (_p27._4.ctor === 'RBNode_elm_builtin') {
											switch (_p27._4._0.ctor) {
												case 'Red':
													if ((_p27._4._3.ctor === 'RBNode_elm_builtin') && (_p27._4._3._0.ctor === 'Red')) {
														break _v36_2;
													} else {
														if ((_p27._4._4.ctor === 'RBNode_elm_builtin') && (_p27._4._4._0.ctor === 'Red')) {
															break _v36_3;
														} else {
															break _v36_6;
														}
													}
												case 'NBlack':
													if (((((_p27._0.ctor === 'BBlack') && (_p27._4._3.ctor === 'RBNode_elm_builtin')) && (_p27._4._3._0.ctor === 'Black')) && (_p27._4._4.ctor === 'RBNode_elm_builtin')) && (_p27._4._4._0.ctor === 'Black')) {
														break _v36_4;
													} else {
														break _v36_6;
													}
												default:
													break _v36_6;
											}
										} else {
											break _v36_6;
										}
									}
								} else {
									break _v36_6;
								}
							} while(false);
							return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._3._1)(_p27._3._3._2)(_p27._3._1)(_p27._3._2)(_p27._1)(_p27._2)(_p27._3._3._3)(_p27._3._3._4)(_p27._3._4)(_p27._4);
						} while(false);
						return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._3._1)(_p27._3._2)(_p27._3._4._1)(_p27._3._4._2)(_p27._1)(_p27._2)(_p27._3._3)(_p27._3._4._3)(_p27._3._4._4)(_p27._4);
					} while(false);
					return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._3._1)(_p27._4._3._2)(_p27._4._1)(_p27._4._2)(_p27._3)(_p27._4._3._3)(_p27._4._3._4)(_p27._4._4);
				} while(false);
				return _elm_lang$core$Dict$balancedTree(_p27._0)(_p27._1)(_p27._2)(_p27._4._1)(_p27._4._2)(_p27._4._4._1)(_p27._4._4._2)(_p27._3)(_p27._4._3)(_p27._4._4._3)(_p27._4._4._4);
			} while(false);
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_elm_lang$core$Dict$Black,
				_p27._4._3._1,
				_p27._4._3._2,
				A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3, _p27._4._3._3),
				A5(
					_elm_lang$core$Dict$balance,
					_elm_lang$core$Dict$Black,
					_p27._4._1,
					_p27._4._2,
					_p27._4._3._4,
					_elm_lang$core$Dict$redden(_p27._4._4)));
		} while(false);
		return A5(
			_elm_lang$core$Dict$RBNode_elm_builtin,
			_elm_lang$core$Dict$Black,
			_p27._3._4._1,
			_p27._3._4._2,
			A5(
				_elm_lang$core$Dict$balance,
				_elm_lang$core$Dict$Black,
				_p27._3._1,
				_p27._3._2,
				_elm_lang$core$Dict$redden(_p27._3._3),
				_p27._3._4._3),
			A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p27._1, _p27._2, _p27._3._4._4, _p27._4));
	} while(false);
	return tree;
};
var _elm_lang$core$Dict$balance = F5(
	function (c, k, v, l, r) {
		var tree = A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
		return _elm_lang$core$Dict$blackish(tree) ? _elm_lang$core$Dict$balanceHelp(tree) : tree;
	});
var _elm_lang$core$Dict$bubble = F5(
	function (c, k, v, l, r) {
		return (_elm_lang$core$Dict$isBBlack(l) || _elm_lang$core$Dict$isBBlack(r)) ? A5(
			_elm_lang$core$Dict$balance,
			_elm_lang$core$Dict$moreBlack(c),
			k,
			v,
			_elm_lang$core$Dict$lessBlackTree(l),
			_elm_lang$core$Dict$lessBlackTree(r)) : A5(_elm_lang$core$Dict$RBNode_elm_builtin, c, k, v, l, r);
	});
var _elm_lang$core$Dict$removeMax = F5(
	function (c, k, v, l, r) {
		var _p28 = r;
		if (_p28.ctor === 'RBEmpty_elm_builtin') {
			return A3(_elm_lang$core$Dict$rem, c, l, r);
		} else {
			return A5(
				_elm_lang$core$Dict$bubble,
				c,
				k,
				v,
				l,
				A5(_elm_lang$core$Dict$removeMax, _p28._0, _p28._1, _p28._2, _p28._3, _p28._4));
		}
	});
var _elm_lang$core$Dict$rem = F3(
	function (color, left, right) {
		var _p29 = {ctor: '_Tuple2', _0: left, _1: right};
		if (_p29._0.ctor === 'RBEmpty_elm_builtin') {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p30 = color;
				switch (_p30.ctor) {
					case 'Red':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
					case 'Black':
						return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBBlack);
					default:
						return _elm_lang$core$Native_Debug.crash('cannot have bblack or nblack nodes at this point');
				}
			} else {
				var _p33 = _p29._1._0;
				var _p32 = _p29._0._0;
				var _p31 = {ctor: '_Tuple3', _0: color, _1: _p32, _2: _p33};
				if ((((_p31.ctor === '_Tuple3') && (_p31._0.ctor === 'Black')) && (_p31._1.ctor === 'LBlack')) && (_p31._2.ctor === 'Red')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._1._1, _p29._1._2, _p29._1._3, _p29._1._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/LBlack/Red',
						color,
						_elm_lang$core$Basics$toString(_p32),
						_elm_lang$core$Basics$toString(_p33));
				}
			}
		} else {
			if (_p29._1.ctor === 'RBEmpty_elm_builtin') {
				var _p36 = _p29._1._0;
				var _p35 = _p29._0._0;
				var _p34 = {ctor: '_Tuple3', _0: color, _1: _p35, _2: _p36};
				if ((((_p34.ctor === '_Tuple3') && (_p34._0.ctor === 'Black')) && (_p34._1.ctor === 'Red')) && (_p34._2.ctor === 'LBlack')) {
					return A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Black, _p29._0._1, _p29._0._2, _p29._0._3, _p29._0._4);
				} else {
					return A4(
						_elm_lang$core$Dict$reportRemBug,
						'Black/Red/LBlack',
						color,
						_elm_lang$core$Basics$toString(_p35),
						_elm_lang$core$Basics$toString(_p36));
				}
			} else {
				var _p40 = _p29._0._2;
				var _p39 = _p29._0._4;
				var _p38 = _p29._0._1;
				var newLeft = A5(_elm_lang$core$Dict$removeMax, _p29._0._0, _p38, _p40, _p29._0._3, _p39);
				var _p37 = A3(_elm_lang$core$Dict$maxWithDefault, _p38, _p40, _p39);
				var k = _p37._0;
				var v = _p37._1;
				return A5(_elm_lang$core$Dict$bubble, color, k, v, newLeft, right);
			}
		}
	});
var _elm_lang$core$Dict$map = F2(
	function (f, dict) {
		var _p41 = dict;
		if (_p41.ctor === 'RBEmpty_elm_builtin') {
			return _elm_lang$core$Dict$RBEmpty_elm_builtin(_elm_lang$core$Dict$LBlack);
		} else {
			var _p42 = _p41._1;
			return A5(
				_elm_lang$core$Dict$RBNode_elm_builtin,
				_p41._0,
				_p42,
				A2(f, _p42, _p41._2),
				A2(_elm_lang$core$Dict$map, f, _p41._3),
				A2(_elm_lang$core$Dict$map, f, _p41._4));
		}
	});
var _elm_lang$core$Dict$Same = {ctor: 'Same'};
var _elm_lang$core$Dict$Remove = {ctor: 'Remove'};
var _elm_lang$core$Dict$Insert = {ctor: 'Insert'};
var _elm_lang$core$Dict$update = F3(
	function (k, alter, dict) {
		var up = function (dict) {
			var _p43 = dict;
			if (_p43.ctor === 'RBEmpty_elm_builtin') {
				var _p44 = alter(_elm_lang$core$Maybe$Nothing);
				if (_p44.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: _elm_lang$core$Dict$Same, _1: _elm_lang$core$Dict$empty};
				} else {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Dict$Insert,
						_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _elm_lang$core$Dict$Red, k, _p44._0, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty)
					};
				}
			} else {
				var _p55 = _p43._2;
				var _p54 = _p43._4;
				var _p53 = _p43._3;
				var _p52 = _p43._1;
				var _p51 = _p43._0;
				var _p45 = A2(_elm_lang$core$Basics$compare, k, _p52);
				switch (_p45.ctor) {
					case 'EQ':
						var _p46 = alter(
							_elm_lang$core$Maybe$Just(_p55));
						if (_p46.ctor === 'Nothing') {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Remove,
								_1: A3(_elm_lang$core$Dict$rem, _p51, _p53, _p54)
							};
						} else {
							return {
								ctor: '_Tuple2',
								_0: _elm_lang$core$Dict$Same,
								_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p46._0, _p53, _p54)
							};
						}
					case 'LT':
						var _p47 = up(_p53);
						var flag = _p47._0;
						var newLeft = _p47._1;
						var _p48 = flag;
						switch (_p48.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, newLeft, _p54)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, newLeft, _p54)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, newLeft, _p54)
								};
						}
					default:
						var _p49 = up(_p54);
						var flag = _p49._0;
						var newRight = _p49._1;
						var _p50 = flag;
						switch (_p50.ctor) {
							case 'Same':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Same,
									_1: A5(_elm_lang$core$Dict$RBNode_elm_builtin, _p51, _p52, _p55, _p53, newRight)
								};
							case 'Insert':
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Insert,
									_1: A5(_elm_lang$core$Dict$balance, _p51, _p52, _p55, _p53, newRight)
								};
							default:
								return {
									ctor: '_Tuple2',
									_0: _elm_lang$core$Dict$Remove,
									_1: A5(_elm_lang$core$Dict$bubble, _p51, _p52, _p55, _p53, newRight)
								};
						}
				}
			}
		};
		var _p56 = up(dict);
		var flag = _p56._0;
		var updatedDict = _p56._1;
		var _p57 = flag;
		switch (_p57.ctor) {
			case 'Same':
				return updatedDict;
			case 'Insert':
				return _elm_lang$core$Dict$ensureBlackRoot(updatedDict);
			default:
				return _elm_lang$core$Dict$blacken(updatedDict);
		}
	});
var _elm_lang$core$Dict$insert = F3(
	function (key, value, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(
				_elm_lang$core$Maybe$Just(value)),
			dict);
	});
var _elm_lang$core$Dict$singleton = F2(
	function (key, value) {
		return A3(_elm_lang$core$Dict$insert, key, value, _elm_lang$core$Dict$empty);
	});
var _elm_lang$core$Dict$union = F2(
	function (t1, t2) {
		return A3(_elm_lang$core$Dict$foldl, _elm_lang$core$Dict$insert, t2, t1);
	});
var _elm_lang$core$Dict$filter = F2(
	function (predicate, dictionary) {
		var add = F3(
			function (key, value, dict) {
				return A2(predicate, key, value) ? A3(_elm_lang$core$Dict$insert, key, value, dict) : dict;
			});
		return A3(_elm_lang$core$Dict$foldl, add, _elm_lang$core$Dict$empty, dictionary);
	});
var _elm_lang$core$Dict$intersect = F2(
	function (t1, t2) {
		return A2(
			_elm_lang$core$Dict$filter,
			F2(
				function (k, _p58) {
					return A2(_elm_lang$core$Dict$member, k, t2);
				}),
			t1);
	});
var _elm_lang$core$Dict$partition = F2(
	function (predicate, dict) {
		var add = F3(
			function (key, value, _p59) {
				var _p60 = _p59;
				var _p62 = _p60._1;
				var _p61 = _p60._0;
				return A2(predicate, key, value) ? {
					ctor: '_Tuple2',
					_0: A3(_elm_lang$core$Dict$insert, key, value, _p61),
					_1: _p62
				} : {
					ctor: '_Tuple2',
					_0: _p61,
					_1: A3(_elm_lang$core$Dict$insert, key, value, _p62)
				};
			});
		return A3(
			_elm_lang$core$Dict$foldl,
			add,
			{ctor: '_Tuple2', _0: _elm_lang$core$Dict$empty, _1: _elm_lang$core$Dict$empty},
			dict);
	});
var _elm_lang$core$Dict$fromList = function (assocs) {
	return A3(
		_elm_lang$core$List$foldl,
		F2(
			function (_p63, dict) {
				var _p64 = _p63;
				return A3(_elm_lang$core$Dict$insert, _p64._0, _p64._1, dict);
			}),
		_elm_lang$core$Dict$empty,
		assocs);
};
var _elm_lang$core$Dict$remove = F2(
	function (key, dict) {
		return A3(
			_elm_lang$core$Dict$update,
			key,
			_elm_lang$core$Basics$always(_elm_lang$core$Maybe$Nothing),
			dict);
	});
var _elm_lang$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			_elm_lang$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2(_elm_lang$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});

//import Maybe, Native.Array, Native.List, Native.Utils, Result //

var _elm_lang$core$Native_Json = function() {


// CORE DECODERS

function succeed(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'succeed',
		msg: msg
	};
}

function fail(msg)
{
	return {
		ctor: '<decoder>',
		tag: 'fail',
		msg: msg
	};
}

function decodePrimitive(tag)
{
	return {
		ctor: '<decoder>',
		tag: tag
	};
}

function decodeContainer(tag, decoder)
{
	return {
		ctor: '<decoder>',
		tag: tag,
		decoder: decoder
	};
}

function decodeNull(value)
{
	return {
		ctor: '<decoder>',
		tag: 'null',
		value: value
	};
}

function decodeField(field, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'field',
		field: field,
		decoder: decoder
	};
}

function decodeIndex(index, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'index',
		index: index,
		decoder: decoder
	};
}

function decodeKeyValuePairs(decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'key-value',
		decoder: decoder
	};
}

function mapMany(f, decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'map-many',
		func: f,
		decoders: decoders
	};
}

function andThen(callback, decoder)
{
	return {
		ctor: '<decoder>',
		tag: 'andThen',
		decoder: decoder,
		callback: callback
	};
}

function oneOf(decoders)
{
	return {
		ctor: '<decoder>',
		tag: 'oneOf',
		decoders: decoders
	};
}


// DECODING OBJECTS

function map1(f, d1)
{
	return mapMany(f, [d1]);
}

function map2(f, d1, d2)
{
	return mapMany(f, [d1, d2]);
}

function map3(f, d1, d2, d3)
{
	return mapMany(f, [d1, d2, d3]);
}

function map4(f, d1, d2, d3, d4)
{
	return mapMany(f, [d1, d2, d3, d4]);
}

function map5(f, d1, d2, d3, d4, d5)
{
	return mapMany(f, [d1, d2, d3, d4, d5]);
}

function map6(f, d1, d2, d3, d4, d5, d6)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6]);
}

function map7(f, d1, d2, d3, d4, d5, d6, d7)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
}

function map8(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
}


// DECODE HELPERS

function ok(value)
{
	return { tag: 'ok', value: value };
}

function badPrimitive(type, value)
{
	return { tag: 'primitive', type: type, value: value };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badField(field, nestedProblems)
{
	return { tag: 'field', field: field, rest: nestedProblems };
}

function badIndex(index, nestedProblems)
{
	return { tag: 'index', index: index, rest: nestedProblems };
}

function badOneOf(problems)
{
	return { tag: 'oneOf', problems: problems };
}

function bad(msg)
{
	return { tag: 'fail', msg: msg };
}

function badToString(problem)
{
	var context = '_';
	while (problem)
	{
		switch (problem.tag)
		{
			case 'primitive':
				return 'Expecting ' + problem.type
					+ (context === '_' ? '' : ' at ' + context)
					+ ' but instead got: ' + jsToString(problem.value);

			case 'index':
				context += '[' + problem.index + ']';
				problem = problem.rest;
				break;

			case 'field':
				context += '.' + problem.field;
				problem = problem.rest;
				break;

			case 'oneOf':
				var problems = problem.problems;
				for (var i = 0; i < problems.length; i++)
				{
					problems[i] = badToString(problems[i]);
				}
				return 'I ran into the following problems'
					+ (context === '_' ? '' : ' at ' + context)
					+ ':\n\n' + problems.join('\n');

			case 'fail':
				return 'I ran into a `fail` decoder'
					+ (context === '_' ? '' : ' at ' + context)
					+ ': ' + problem.msg;
		}
	}
}

function jsToString(value)
{
	return value === undefined
		? 'undefined'
		: JSON.stringify(value);
}


// DECODE

function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}

function runHelp(decoder, value)
{
	switch (decoder.tag)
	{
		case 'bool':
			return (typeof value === 'boolean')
				? ok(value)
				: badPrimitive('a Bool', value);

		case 'int':
			if (typeof value !== 'number') {
				return badPrimitive('an Int', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return ok(value);
			}

			return badPrimitive('an Int', value);

		case 'float':
			return (typeof value === 'number')
				? ok(value)
				: badPrimitive('a Float', value);

		case 'string':
			return (typeof value === 'string')
				? ok(value)
				: (value instanceof String)
					? ok(value + '')
					: badPrimitive('a String', value);

		case 'null':
			return (value === null)
				? ok(decoder.value)
				: badPrimitive('null', value);

		case 'value':
			return ok(value);

		case 'list':
			if (!(value instanceof Array))
			{
				return badPrimitive('a List', value);
			}

			var list = _elm_lang$core$Native_List.Nil;
			for (var i = value.length; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result)
				}
				list = _elm_lang$core$Native_List.Cons(result.value, list);
			}
			return ok(list);

		case 'array':
			if (!(value instanceof Array))
			{
				return badPrimitive('an Array', value);
			}

			var len = value.length;
			var array = new Array(len);
			for (var i = len; i--; )
			{
				var result = runHelp(decoder.decoder, value[i]);
				if (result.tag !== 'ok')
				{
					return badIndex(i, result);
				}
				array[i] = result.value;
			}
			return ok(_elm_lang$core$Native_Array.fromJSArray(array));

		case 'maybe':
			var result = runHelp(decoder.decoder, value);
			return (result.tag === 'ok')
				? ok(_elm_lang$core$Maybe$Just(result.value))
				: ok(_elm_lang$core$Maybe$Nothing);

		case 'field':
			var field = decoder.field;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return badPrimitive('an object with a field named `' + field + '`', value);
			}

			var result = runHelp(decoder.decoder, value[field]);
			return (result.tag === 'ok') ? result : badField(field, result);

		case 'index':
			var index = decoder.index;
			if (!(value instanceof Array))
			{
				return badPrimitive('an array', value);
			}
			if (index >= value.length)
			{
				return badPrimitive('a longer array. Need index ' + index + ' but there are only ' + value.length + ' entries', value);
			}

			var result = runHelp(decoder.decoder, value[index]);
			return (result.tag === 'ok') ? result : badIndex(index, result);

		case 'key-value':
			if (typeof value !== 'object' || value === null || value instanceof Array)
			{
				return badPrimitive('an object', value);
			}

			var keyValuePairs = _elm_lang$core$Native_List.Nil;
			for (var key in value)
			{
				var result = runHelp(decoder.decoder, value[key]);
				if (result.tag !== 'ok')
				{
					return badField(key, result);
				}
				var pair = _elm_lang$core$Native_Utils.Tuple2(key, result.value);
				keyValuePairs = _elm_lang$core$Native_List.Cons(pair, keyValuePairs);
			}
			return ok(keyValuePairs);

		case 'map-many':
			var answer = decoder.func;
			var decoders = decoder.decoders;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = runHelp(decoders[i], value);
				if (result.tag !== 'ok')
				{
					return result;
				}
				answer = answer(result.value);
			}
			return ok(answer);

		case 'andThen':
			var result = runHelp(decoder.decoder, value);
			return (result.tag !== 'ok')
				? result
				: runHelp(decoder.callback(result.value), value);

		case 'oneOf':
			var errors = [];
			var temp = decoder.decoders;
			while (temp.ctor !== '[]')
			{
				var result = runHelp(temp._0, value);

				if (result.tag === 'ok')
				{
					return result;
				}

				errors.push(result);

				temp = temp._1;
			}
			return badOneOf(errors);

		case 'fail':
			return bad(decoder.msg);

		case 'succeed':
			return ok(decoder.msg);
	}
}


// EQUALITY

function equality(a, b)
{
	if (a === b)
	{
		return true;
	}

	if (a.tag !== b.tag)
	{
		return false;
	}

	switch (a.tag)
	{
		case 'succeed':
		case 'fail':
			return a.msg === b.msg;

		case 'bool':
		case 'int':
		case 'float':
		case 'string':
		case 'value':
			return true;

		case 'null':
			return a.value === b.value;

		case 'list':
		case 'array':
		case 'maybe':
		case 'key-value':
			return equality(a.decoder, b.decoder);

		case 'field':
			return a.field === b.field && equality(a.decoder, b.decoder);

		case 'index':
			return a.index === b.index && equality(a.decoder, b.decoder);

		case 'map-many':
			if (a.func !== b.func)
			{
				return false;
			}
			return listEquality(a.decoders, b.decoders);

		case 'andThen':
			return a.callback === b.callback && equality(a.decoder, b.decoder);

		case 'oneOf':
			return listEquality(a.decoders, b.decoders);
	}
}

function listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

function encode(indentLevel, value)
{
	return JSON.stringify(value, null, indentLevel);
}

function identity(value)
{
	return value;
}

function encodeObject(keyValuePairs)
{
	var obj = {};
	while (keyValuePairs.ctor !== '[]')
	{
		var pair = keyValuePairs._0;
		obj[pair._0] = pair._1;
		keyValuePairs = keyValuePairs._1;
	}
	return obj;
}

return {
	encode: F2(encode),
	runOnString: F2(runOnString),
	run: F2(run),

	decodeNull: decodeNull,
	decodePrimitive: decodePrimitive,
	decodeContainer: F2(decodeContainer),

	decodeField: F2(decodeField),
	decodeIndex: F2(decodeIndex),

	map1: F2(map1),
	map2: F3(map2),
	map3: F4(map3),
	map4: F5(map4),
	map5: F6(map5),
	map6: F7(map6),
	map7: F8(map7),
	map8: F9(map8),
	decodeKeyValuePairs: decodeKeyValuePairs,

	andThen: F2(andThen),
	fail: fail,
	succeed: succeed,
	oneOf: oneOf,

	identity: identity,
	encodeNull: null,
	encodeArray: _elm_lang$core$Native_Array.toJSArray,
	encodeList: _elm_lang$core$Native_List.toArray,
	encodeObject: encodeObject,

	equality: equality
};

}();

var _elm_lang$core$Json_Encode$list = _elm_lang$core$Native_Json.encodeList;
var _elm_lang$core$Json_Encode$array = _elm_lang$core$Native_Json.encodeArray;
var _elm_lang$core$Json_Encode$object = _elm_lang$core$Native_Json.encodeObject;
var _elm_lang$core$Json_Encode$null = _elm_lang$core$Native_Json.encodeNull;
var _elm_lang$core$Json_Encode$bool = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$float = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$int = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$string = _elm_lang$core$Native_Json.identity;
var _elm_lang$core$Json_Encode$encode = _elm_lang$core$Native_Json.encode;
var _elm_lang$core$Json_Encode$Value = {ctor: 'Value'};

var _elm_lang$core$Json_Decode$null = _elm_lang$core$Native_Json.decodeNull;
var _elm_lang$core$Json_Decode$value = _elm_lang$core$Native_Json.decodePrimitive('value');
var _elm_lang$core$Json_Decode$andThen = _elm_lang$core$Native_Json.andThen;
var _elm_lang$core$Json_Decode$fail = _elm_lang$core$Native_Json.fail;
var _elm_lang$core$Json_Decode$succeed = _elm_lang$core$Native_Json.succeed;
var _elm_lang$core$Json_Decode$lazy = function (thunk) {
	return A2(
		_elm_lang$core$Json_Decode$andThen,
		thunk,
		_elm_lang$core$Json_Decode$succeed(
			{ctor: '_Tuple0'}));
};
var _elm_lang$core$Json_Decode$decodeValue = _elm_lang$core$Native_Json.run;
var _elm_lang$core$Json_Decode$decodeString = _elm_lang$core$Native_Json.runOnString;
var _elm_lang$core$Json_Decode$map8 = _elm_lang$core$Native_Json.map8;
var _elm_lang$core$Json_Decode$map7 = _elm_lang$core$Native_Json.map7;
var _elm_lang$core$Json_Decode$map6 = _elm_lang$core$Native_Json.map6;
var _elm_lang$core$Json_Decode$map5 = _elm_lang$core$Native_Json.map5;
var _elm_lang$core$Json_Decode$map4 = _elm_lang$core$Native_Json.map4;
var _elm_lang$core$Json_Decode$map3 = _elm_lang$core$Native_Json.map3;
var _elm_lang$core$Json_Decode$map2 = _elm_lang$core$Native_Json.map2;
var _elm_lang$core$Json_Decode$map = _elm_lang$core$Native_Json.map1;
var _elm_lang$core$Json_Decode$oneOf = _elm_lang$core$Native_Json.oneOf;
var _elm_lang$core$Json_Decode$maybe = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'maybe', decoder);
};
var _elm_lang$core$Json_Decode$index = _elm_lang$core$Native_Json.decodeIndex;
var _elm_lang$core$Json_Decode$field = _elm_lang$core$Native_Json.decodeField;
var _elm_lang$core$Json_Decode$at = F2(
	function (fields, decoder) {
		return A3(_elm_lang$core$List$foldr, _elm_lang$core$Json_Decode$field, decoder, fields);
	});
var _elm_lang$core$Json_Decode$keyValuePairs = _elm_lang$core$Native_Json.decodeKeyValuePairs;
var _elm_lang$core$Json_Decode$dict = function (decoder) {
	return A2(
		_elm_lang$core$Json_Decode$map,
		_elm_lang$core$Dict$fromList,
		_elm_lang$core$Json_Decode$keyValuePairs(decoder));
};
var _elm_lang$core$Json_Decode$array = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'array', decoder);
};
var _elm_lang$core$Json_Decode$list = function (decoder) {
	return A2(_elm_lang$core$Native_Json.decodeContainer, 'list', decoder);
};
var _elm_lang$core$Json_Decode$nullable = function (decoder) {
	return _elm_lang$core$Json_Decode$oneOf(
		{
			ctor: '::',
			_0: _elm_lang$core$Json_Decode$null(_elm_lang$core$Maybe$Nothing),
			_1: {
				ctor: '::',
				_0: A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, decoder),
				_1: {ctor: '[]'}
			}
		});
};
var _elm_lang$core$Json_Decode$float = _elm_lang$core$Native_Json.decodePrimitive('float');
var _elm_lang$core$Json_Decode$int = _elm_lang$core$Native_Json.decodePrimitive('int');
var _elm_lang$core$Json_Decode$bool = _elm_lang$core$Native_Json.decodePrimitive('bool');
var _elm_lang$core$Json_Decode$string = _elm_lang$core$Native_Json.decodePrimitive('string');
var _elm_lang$core$Json_Decode$Decoder = {ctor: 'Decoder'};

var _elm_lang$core$Debug$crash = _elm_lang$core$Native_Debug.crash;
var _elm_lang$core$Debug$log = _elm_lang$core$Native_Debug.log;

var _elm_lang$core$Tuple$mapSecond = F2(
	function (func, _p0) {
		var _p1 = _p0;
		return {
			ctor: '_Tuple2',
			_0: _p1._0,
			_1: func(_p1._1)
		};
	});
var _elm_lang$core$Tuple$mapFirst = F2(
	function (func, _p2) {
		var _p3 = _p2;
		return {
			ctor: '_Tuple2',
			_0: func(_p3._0),
			_1: _p3._1
		};
	});
var _elm_lang$core$Tuple$second = function (_p4) {
	var _p5 = _p4;
	return _p5._1;
};
var _elm_lang$core$Tuple$first = function (_p6) {
	var _p7 = _p6;
	return _p7._0;
};

//import //

var _elm_lang$core$Native_Platform = function() {


// PROGRAMS

function program(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flags !== 'undefined')
				{
					throw new Error(
						'The `' + moduleName + '` module does not need flags.\n'
						+ 'Call ' + moduleName + '.worker() with no arguments and you should be all set!'
					);
				}

				return initialize(
					impl.init,
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function programWithFlags(impl)
{
	return function(flagDecoder)
	{
		return function(object, moduleName)
		{
			object['worker'] = function worker(flags)
			{
				if (typeof flagDecoder === 'undefined')
				{
					throw new Error(
						'Are you trying to sneak a Never value into Elm? Trickster!\n'
						+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
						+ 'Use `program` instead if you do not want flags.'
					);
				}

				var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
				if (result.ctor === 'Err')
				{
					throw new Error(
						moduleName + '.worker(...) was called with an unexpected argument.\n'
						+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
						+ result._0
					);
				}

				return initialize(
					impl.init(result._0),
					impl.update,
					impl.subscriptions,
					renderer
				);
			};
		};
	};
}

function renderer(enqueue, _)
{
	return function(_) {};
}


// HTML TO PROGRAM

function htmlToProgram(vnode)
{
	var emptyBag = batch(_elm_lang$core$Native_List.Nil);
	var noChange = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		emptyBag
	);

	return _elm_lang$virtual_dom$VirtualDom$program({
		init: noChange,
		view: function(model) { return main; },
		update: F2(function(msg, model) { return noChange; }),
		subscriptions: function (model) { return emptyBag; }
	});
}


// INITIALIZE A PROGRAM

function initialize(init, update, subscriptions, renderer)
{
	// ambient state
	var managers = {};
	var updateView;

	// init and update state in main process
	var initApp = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		var model = init._0;
		updateView = renderer(enqueue, model);
		var cmds = init._1;
		var subs = subscriptions(model);
		dispatchEffects(managers, cmds, subs);
		callback(_elm_lang$core$Native_Scheduler.succeed(model));
	});

	function onMessage(msg, model)
	{
		return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
			var results = A2(update, msg, model);
			model = results._0;
			updateView(model);
			var cmds = results._1;
			var subs = subscriptions(model);
			dispatchEffects(managers, cmds, subs);
			callback(_elm_lang$core$Native_Scheduler.succeed(model));
		});
	}

	var mainProcess = spawnLoop(initApp, onMessage);

	function enqueue(msg)
	{
		_elm_lang$core$Native_Scheduler.rawSend(mainProcess, msg);
	}

	var ports = setupEffects(managers, enqueue);

	return ports ? { ports: ports } : {};
}


// EFFECT MANAGERS

var effectManagers = {};

function setupEffects(managers, callback)
{
	var ports;

	// setup all necessary effect managers
	for (var key in effectManagers)
	{
		var manager = effectManagers[key];

		if (manager.isForeign)
		{
			ports = ports || {};
			ports[key] = manager.tag === 'cmd'
				? setupOutgoingPort(key)
				: setupIncomingPort(key, callback);
		}

		managers[key] = makeManager(manager, callback);
	}

	return ports;
}

function makeManager(info, callback)
{
	var router = {
		main: callback,
		self: undefined
	};

	var tag = info.tag;
	var onEffects = info.onEffects;
	var onSelfMsg = info.onSelfMsg;

	function onMessage(msg, state)
	{
		if (msg.ctor === 'self')
		{
			return A3(onSelfMsg, router, msg._0, state);
		}

		var fx = msg._0;
		switch (tag)
		{
			case 'cmd':
				return A3(onEffects, router, fx.cmds, state);

			case 'sub':
				return A3(onEffects, router, fx.subs, state);

			case 'fx':
				return A4(onEffects, router, fx.cmds, fx.subs, state);
		}
	}

	var process = spawnLoop(info.init, onMessage);
	router.self = process;
	return process;
}

function sendToApp(router, msg)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		router.main(msg);
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sendToSelf(router, msg)
{
	return A2(_elm_lang$core$Native_Scheduler.send, router.self, {
		ctor: 'self',
		_0: msg
	});
}


// HELPER for STATEFUL LOOPS

function spawnLoop(init, onMessage)
{
	var andThen = _elm_lang$core$Native_Scheduler.andThen;

	function loop(state)
	{
		var handleMsg = _elm_lang$core$Native_Scheduler.receive(function(msg) {
			return onMessage(msg, state);
		});
		return A2(andThen, loop, handleMsg);
	}

	var task = A2(andThen, loop, init);

	return _elm_lang$core$Native_Scheduler.rawSpawn(task);
}


// BAGS

function leaf(home)
{
	return function(value)
	{
		return {
			type: 'leaf',
			home: home,
			value: value
		};
	};
}

function batch(list)
{
	return {
		type: 'node',
		branches: list
	};
}

function map(tagger, bag)
{
	return {
		type: 'map',
		tagger: tagger,
		tree: bag
	}
}


// PIPE BAGS INTO EFFECT MANAGERS

function dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	gatherEffects(true, cmdBag, effectsDict, null);
	gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		var fx = home in effectsDict
			? effectsDict[home]
			: {
				cmds: _elm_lang$core$Native_List.Nil,
				subs: _elm_lang$core$Native_List.Nil
			};

		_elm_lang$core$Native_Scheduler.rawSend(managers[home], { ctor: 'fx', _0: fx });
	}
}

function gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.type)
	{
		case 'leaf':
			var home = bag.home;
			var effect = toEffect(isCmd, home, taggers, bag.value);
			effectsDict[home] = insert(isCmd, effect, effectsDict[home]);
			return;

		case 'node':
			var list = bag.branches;
			while (list.ctor !== '[]')
			{
				gatherEffects(isCmd, list._0, effectsDict, taggers);
				list = list._1;
			}
			return;

		case 'map':
			gatherEffects(isCmd, bag.tree, effectsDict, {
				tagger: bag.tagger,
				rest: taggers
			});
			return;
	}
}

function toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		var temp = taggers;
		while (temp)
		{
			x = temp.tagger(x);
			temp = temp.rest;
		}
		return x;
	}

	var map = isCmd
		? effectManagers[home].cmdMap
		: effectManagers[home].subMap;

	return A2(map, applyTaggers, value)
}

function insert(isCmd, newEffect, effects)
{
	effects = effects || {
		cmds: _elm_lang$core$Native_List.Nil,
		subs: _elm_lang$core$Native_List.Nil
	};
	if (isCmd)
	{
		effects.cmds = _elm_lang$core$Native_List.Cons(newEffect, effects.cmds);
		return effects;
	}
	effects.subs = _elm_lang$core$Native_List.Cons(newEffect, effects.subs);
	return effects;
}


// PORTS

function checkPortName(name)
{
	if (name in effectManagers)
	{
		throw new Error('There can only be one port named `' + name + '`, but your program has multiple.');
	}
}


// OUTGOING PORTS

function outgoingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'cmd',
		cmdMap: outgoingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var outgoingPortMap = F2(function cmdMap(tagger, value) {
	return value;
});

function setupOutgoingPort(name)
{
	var subs = [];
	var converter = effectManagers[name].converter;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function onEffects(router, cmdList, state)
	{
		while (cmdList.ctor !== '[]')
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = converter(cmdList._0);
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
			cmdList = cmdList._1;
		}
		return init;
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}


// INCOMING PORTS

function incomingPort(name, converter)
{
	checkPortName(name);
	effectManagers[name] = {
		tag: 'sub',
		subMap: incomingPortMap,
		converter: converter,
		isForeign: true
	};
	return leaf(name);
}

var incomingPortMap = F2(function subMap(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});

function setupIncomingPort(name, callback)
{
	var sentBeforeInit = [];
	var subs = _elm_lang$core$Native_List.Nil;
	var converter = effectManagers[name].converter;
	var currentOnEffects = preInitOnEffects;
	var currentSend = preInitSend;

	// CREATE MANAGER

	var init = _elm_lang$core$Native_Scheduler.succeed(null);

	function preInitOnEffects(router, subList, state)
	{
		var postInitResult = postInitOnEffects(router, subList, state);

		for(var i = 0; i < sentBeforeInit.length; i++)
		{
			postInitSend(sentBeforeInit[i]);
		}

		sentBeforeInit = null; // to release objects held in queue
		currentSend = postInitSend;
		currentOnEffects = postInitOnEffects;
		return postInitResult;
	}

	function postInitOnEffects(router, subList, state)
	{
		subs = subList;
		return init;
	}

	function onEffects(router, subList, state)
	{
		return currentOnEffects(router, subList, state);
	}

	effectManagers[name].init = init;
	effectManagers[name].onEffects = F3(onEffects);

	// PUBLIC API

	function preInitSend(value)
	{
		sentBeforeInit.push(value);
	}

	function postInitSend(value)
	{
		var temp = subs;
		while (temp.ctor !== '[]')
		{
			callback(temp._0(value));
			temp = temp._1;
		}
	}

	function send(incomingValue)
	{
		var result = A2(_elm_lang$core$Json_Decode$decodeValue, converter, incomingValue);
		if (result.ctor === 'Err')
		{
			throw new Error('Trying to send an unexpected type of value through port `' + name + '`:\n' + result._0);
		}

		currentSend(result._0);
	}

	return { send: send };
}

return {
	// routers
	sendToApp: F2(sendToApp),
	sendToSelf: F2(sendToSelf),

	// global setup
	effectManagers: effectManagers,
	outgoingPort: outgoingPort,
	incomingPort: incomingPort,

	htmlToProgram: htmlToProgram,
	program: program,
	programWithFlags: programWithFlags,
	initialize: initialize,

	// effect bags
	leaf: leaf,
	batch: batch,
	map: F2(map)
};

}();

//import Native.Utils //

var _elm_lang$core$Native_Scheduler = function() {

var MAX_STEPS = 10000;


// TASKS

function succeed(value)
{
	return {
		ctor: '_Task_succeed',
		value: value
	};
}

function fail(error)
{
	return {
		ctor: '_Task_fail',
		value: error
	};
}

function nativeBinding(callback)
{
	return {
		ctor: '_Task_nativeBinding',
		callback: callback,
		cancel: null
	};
}

function andThen(callback, task)
{
	return {
		ctor: '_Task_andThen',
		callback: callback,
		task: task
	};
}

function onError(callback, task)
{
	return {
		ctor: '_Task_onError',
		callback: callback,
		task: task
	};
}

function receive(callback)
{
	return {
		ctor: '_Task_receive',
		callback: callback
	};
}


// PROCESSES

function rawSpawn(task)
{
	var process = {
		ctor: '_Process',
		id: _elm_lang$core$Native_Utils.guid(),
		root: task,
		stack: null,
		mailbox: []
	};

	enqueue(process);

	return process;
}

function spawn(task)
{
	return nativeBinding(function(callback) {
		var process = rawSpawn(task);
		callback(succeed(process));
	});
}

function rawSend(process, msg)
{
	process.mailbox.push(msg);
	enqueue(process);
}

function send(process, msg)
{
	return nativeBinding(function(callback) {
		rawSend(process, msg);
		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function kill(process)
{
	return nativeBinding(function(callback) {
		var root = process.root;
		if (root.ctor === '_Task_nativeBinding' && root.cancel)
		{
			root.cancel();
		}

		process.root = null;

		callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}

function sleep(time)
{
	return nativeBinding(function(callback) {
		var id = setTimeout(function() {
			callback(succeed(_elm_lang$core$Native_Utils.Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}


// STEP PROCESSES

function step(numSteps, process)
{
	while (numSteps < MAX_STEPS)
	{
		var ctor = process.root.ctor;

		if (ctor === '_Task_succeed')
		{
			while (process.stack && process.stack.ctor === '_Task_onError')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_fail')
		{
			while (process.stack && process.stack.ctor === '_Task_andThen')
			{
				process.stack = process.stack.rest;
			}
			if (process.stack === null)
			{
				break;
			}
			process.root = process.stack.callback(process.root.value);
			process.stack = process.stack.rest;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_andThen')
		{
			process.stack = {
				ctor: '_Task_andThen',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_onError')
		{
			process.stack = {
				ctor: '_Task_onError',
				callback: process.root.callback,
				rest: process.stack
			};
			process.root = process.root.task;
			++numSteps;
			continue;
		}

		if (ctor === '_Task_nativeBinding')
		{
			process.root.cancel = process.root.callback(function(newRoot) {
				process.root = newRoot;
				enqueue(process);
			});

			break;
		}

		if (ctor === '_Task_receive')
		{
			var mailbox = process.mailbox;
			if (mailbox.length === 0)
			{
				break;
			}

			process.root = process.root.callback(mailbox.shift());
			++numSteps;
			continue;
		}

		throw new Error(ctor);
	}

	if (numSteps < MAX_STEPS)
	{
		return numSteps + 1;
	}
	enqueue(process);

	return numSteps;
}


// WORK QUEUE

var working = false;
var workQueue = [];

function enqueue(process)
{
	workQueue.push(process);

	if (!working)
	{
		setTimeout(work, 0);
		working = true;
	}
}

function work()
{
	var numSteps = 0;
	var process;
	while (numSteps < MAX_STEPS && (process = workQueue.shift()))
	{
		if (process.root)
		{
			numSteps = step(numSteps, process);
		}
	}
	if (!process)
	{
		working = false;
		return;
	}
	setTimeout(work, 0);
}


return {
	succeed: succeed,
	fail: fail,
	nativeBinding: nativeBinding,
	andThen: F2(andThen),
	onError: F2(onError),
	receive: receive,

	spawn: spawn,
	kill: kill,
	sleep: sleep,
	send: F2(send),

	rawSpawn: rawSpawn,
	rawSend: rawSend
};

}();
var _elm_lang$core$Platform_Cmd$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Cmd$none = _elm_lang$core$Platform_Cmd$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Cmd_ops = _elm_lang$core$Platform_Cmd_ops || {};
_elm_lang$core$Platform_Cmd_ops['!'] = F2(
	function (model, commands) {
		return {
			ctor: '_Tuple2',
			_0: model,
			_1: _elm_lang$core$Platform_Cmd$batch(commands)
		};
	});
var _elm_lang$core$Platform_Cmd$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Cmd$Cmd = {ctor: 'Cmd'};

var _elm_lang$core$Platform_Sub$batch = _elm_lang$core$Native_Platform.batch;
var _elm_lang$core$Platform_Sub$none = _elm_lang$core$Platform_Sub$batch(
	{ctor: '[]'});
var _elm_lang$core$Platform_Sub$map = _elm_lang$core$Native_Platform.map;
var _elm_lang$core$Platform_Sub$Sub = {ctor: 'Sub'};

var _elm_lang$core$Platform$hack = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Platform$sendToSelf = _elm_lang$core$Native_Platform.sendToSelf;
var _elm_lang$core$Platform$sendToApp = _elm_lang$core$Native_Platform.sendToApp;
var _elm_lang$core$Platform$programWithFlags = _elm_lang$core$Native_Platform.programWithFlags;
var _elm_lang$core$Platform$program = _elm_lang$core$Native_Platform.program;
var _elm_lang$core$Platform$Program = {ctor: 'Program'};
var _elm_lang$core$Platform$Task = {ctor: 'Task'};
var _elm_lang$core$Platform$ProcessId = {ctor: 'ProcessId'};
var _elm_lang$core$Platform$Router = {ctor: 'Router'};

var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode = _elm_lang$core$Json_Decode$succeed;
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$resolve = _elm_lang$core$Json_Decode$andThen(_elm_lang$core$Basics$identity);
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom = _elm_lang$core$Json_Decode$map2(
	F2(
		function (x, y) {
			return y(x);
		}));
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$hardcoded = function (_p0) {
	return _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom(
		_elm_lang$core$Json_Decode$succeed(_p0));
};
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder = F3(
	function (pathDecoder, valDecoder, fallback) {
		var nullOr = function (decoder) {
			return _elm_lang$core$Json_Decode$oneOf(
				{
					ctor: '::',
					_0: decoder,
					_1: {
						ctor: '::',
						_0: _elm_lang$core$Json_Decode$null(fallback),
						_1: {ctor: '[]'}
					}
				});
		};
		var handleResult = function (input) {
			var _p1 = A2(_elm_lang$core$Json_Decode$decodeValue, pathDecoder, input);
			if (_p1.ctor === 'Ok') {
				var _p2 = A2(
					_elm_lang$core$Json_Decode$decodeValue,
					nullOr(valDecoder),
					_p1._0);
				if (_p2.ctor === 'Ok') {
					return _elm_lang$core$Json_Decode$succeed(_p2._0);
				} else {
					return _elm_lang$core$Json_Decode$fail(_p2._0);
				}
			} else {
				return _elm_lang$core$Json_Decode$succeed(fallback);
			}
		};
		return A2(_elm_lang$core$Json_Decode$andThen, handleResult, _elm_lang$core$Json_Decode$value);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalAt = F4(
	function (path, valDecoder, fallback, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder,
				A2(_elm_lang$core$Json_Decode$at, path, _elm_lang$core$Json_Decode$value),
				valDecoder,
				fallback),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional = F4(
	function (key, valDecoder, fallback, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optionalDecoder,
				A2(_elm_lang$core$Json_Decode$field, key, _elm_lang$core$Json_Decode$value),
				valDecoder,
				fallback),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt = F3(
	function (path, valDecoder, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A2(_elm_lang$core$Json_Decode$at, path, valDecoder),
			decoder);
	});
var _NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$custom,
			A2(_elm_lang$core$Json_Decode$field, key, valDecoder),
			decoder);
	});

var _danielnarey$elm_bulma_classes$Internal_Types$Bulma = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return function (l) {
												return function (m) {
													return function (n) {
														return function (o) {
															return function (p) {
																return function (q) {
																	return function (r) {
																		return function (s) {
																			return function (t) {
																				return function (u) {
																					return function (v) {
																						return function (w) {
																							return function (x) {
																								return function (y) {
																									return function (z) {
																										return function (_1) {
																											return function (_2) {
																												return function (_3) {
																													return function (_4) {
																														return function (_5) {
																															return function (_6) {
																																return function (_7) {
																																	return function (_8) {
																																		return function (_9) {
																																			return function (_10) {
																																				return function (_11) {
																																					return function (_12) {
																																						return {properties: a, box: b, content: c, image: d, heading: e, icon: f, field: g, control: h, button: i, $delete: j, input: k, textarea: l, checkbox: m, radio: n, select: o, label: p, help: q, notification: r, progress: s, tag: t, number: u, section: v, footer: w, nav: x, hero: y, level: z, feature: _1, columns: _2, tile: _3, table: _4, menu: _5, tabs: _6, panel: _7, pagination: _8, card: _9, media: _10, message: _11, modal: _12};
																																					};
																																				};
																																			};
																																		};
																																	};
																																};
																															};
																														};
																													};
																												};
																											};
																										};
																									};
																								};
																							};
																						};
																					};
																				};
																			};
																		};
																	};
																};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _danielnarey$elm_bulma_classes$Internal_Types$Properties = F6(
	function (a, b, c, d, e, f) {
		return {$float: a, alignment: b, sizing: c, display: d, visibility: e, interaction: f};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Box = function (a) {
	return {container: a};
};
var _danielnarey$elm_bulma_classes$Internal_Types$Content = F2(
	function (a, b) {
		return {container: a, size: b};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Image = F2(
	function (a, b) {
		return {container: a, size: b};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Heading = F4(
	function (a, b, c, d) {
		return {title: a, subtitle: b, size: c, spacing: d};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Icon = F2(
	function (a, b) {
		return {container: a, size: b};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Field = F6(
	function (a, b, c, d, e, f) {
		return {container: a, label: b, body: c, isGrouped: d, hasAddons: e, layout: f};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Control = F4(
	function (a, b, c, d) {
		return {container: a, hasIcons: b, state: c, sizing: d};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Button = F6(
	function (a, b, c, d, e, f) {
		return {ui: a, style: b, size: c, state: d, color: e, addon: f};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Input = F6(
	function (a, b, c, d, e, f) {
		return {ui: a, display: b, size: c, state: d, color: e, addon: f};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Textarea = F5(
	function (a, b, c, d, e) {
		return {ui: a, display: b, size: c, state: d, color: e};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Checkbox = function (a) {
	return {ui: a};
};
var _danielnarey$elm_bulma_classes$Internal_Types$Radio = function (a) {
	return {ui: a};
};
var _danielnarey$elm_bulma_classes$Internal_Types$Select = F4(
	function (a, b, c, d) {
		return {ui: a, size: b, state: c, addon: d};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Label = F2(
	function (a, b) {
		return {ui: a, size: b};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Help = F2(
	function (a, b) {
		return {ui: a, color: b};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Delete = F2(
	function (a, b) {
		return {ui: a, size: b};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Notification = F2(
	function (a, b) {
		return {ui: a, color: b};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Progress = F3(
	function (a, b, c) {
		return {ui: a, size: b, color: c};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Tag = F3(
	function (a, b, c) {
		return {ui: a, size: b, color: c};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Number = function (a) {
	return {ui: a};
};
var _danielnarey$elm_bulma_classes$Internal_Types$Section = F2(
	function (a, b) {
		return {container: a, spacing: b};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Nav = F8(
	function (a, b, c, d, e, f, g, h) {
		return {container: a, left: b, center: c, right: d, toggle: e, menu: f, item: g, style: h};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Footer = function (a) {
	return {container: a};
};
var _danielnarey$elm_bulma_classes$Internal_Types$Hero = F9(
	function (a, b, c, d, e, f, g, h, i) {
		return {container: a, head: b, body: c, foot: d, video: e, buttons: f, style: g, size: h, color: i};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Feature = F2(
	function (a, b) {
		return {container: a, sizing: b};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Columns = F5(
	function (a, b, c, d, e) {
		return {container: a, column: b, alignment: c, spacing: d, display: e};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Level = F5(
	function (a, b, c, d, e) {
		return {container: a, left: b, right: c, item: d, mobile: e};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Table = F4(
	function (a, b, c, d) {
		return {container: a, row: b, style: c, spacing: d};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Tile = F4(
	function (a, b, c, d) {
		return {container: a, level: b, orientation: c, width: d};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Menu = F3(
	function (a, b, c) {
		return {container: a, label: b, list: c};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Tabs = F5(
	function (a, b, c, d, e) {
		return {container: a, tab: b, style: c, alignment: d, size: e};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Panel = F4(
	function (a, b, c, d) {
		return {container: a, heading: b, tabs: c, block: d};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Pagination = F6(
	function (a, b, c, d, e, f) {
		return {container: a, previous: b, next: c, list: d, position: e, size: f};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Card = F5(
	function (a, b, c, d, e) {
		return {container: a, image: b, content: c, header: d, footer: e};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Media = F5(
	function (a, b, c, d, e) {
		return {container: a, left: b, right: c, content: d, size: e};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Message = F4(
	function (a, b, c, d) {
		return {container: a, header: b, body: c, color: d};
	});
var _danielnarey$elm_bulma_classes$Internal_Types$Modal = F6(
	function (a, b, c, d, e, f) {
		return {container: a, background: b, content: c, close: d, card: e, state: f};
	});

var _danielnarey$elm_bulma_classes$Internal_Classes$modal = {
	container: 'modal',
	background: 'modal-background',
	content: 'modal-content',
	close: {
		ui: 'modal-close',
		size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large'}
	},
	card: {container: 'modal-card', head: 'modal-card-head', title: 'modal-card-title', body: 'modal-card-body', foot: 'modal-card-foot'},
	state: {isActive: 'is-active'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$message = {
	container: 'message',
	header: 'message-header',
	body: 'message-body',
	color: {isPrimary: 'is-primary', isInfo: 'is-info', isSuccess: 'is-success', isWarning: 'is-warning', isDanger: 'is-danger', isWhite: 'is-white', isLight: 'is-light', isDark: 'is-dark', isBlack: 'is-black'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$media = {
	container: 'media',
	left: 'media-left',
	right: 'media-right',
	content: 'media-content',
	size: {isLarge: 'is-large'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$card = {
	container: 'card',
	image: 'card-image',
	content: 'card-content',
	header: {container: 'card-header', title: 'card-header-title', icon: 'card-header-icon'},
	footer: {container: 'card-footer', item: 'card-footer-item'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$box = {container: 'box'};
var _danielnarey$elm_bulma_classes$Internal_Classes$pagination = {
	container: 'pagination',
	previous: 'pagination-previous',
	next: 'pagination-next',
	list: {
		container: 'pagination-list',
		link: {
			ui: 'pagination-link',
			state: {isCurrent: 'is-currrent'}
		},
		ellipsis: 'pagination-ellipsis'
	},
	position: {isCentered: 'is-centered', isRight: 'is-right'},
	size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$panel = {
	container: 'panel',
	heading: 'panel-heading',
	tabs: {
		container: 'panel-tabs',
		tab: {
			state: {isActive: 'is-active'}
		}
	},
	block: {
		container: 'panel-block',
		icon: 'panel-icon',
		list: 'panel-list',
		state: {isActive: 'is-active'}
	}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$tabs = {
	container: 'tabs',
	tab: {
		state: {isActive: 'is-active'}
	},
	style: {isBoxed: 'is-boxed', isToggle: 'is-toggle'},
	alignment: {isCentered: 'is-centered', isRight: 'is-right'},
	size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large', isFullWidth: 'is-fullwidth'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$menu = {container: 'menu', label: 'menu-label', list: 'menu-list'};
var _danielnarey$elm_bulma_classes$Internal_Classes$table = {
	container: 'table',
	row: {
		state: {isSelected: 'is-selected'}
	},
	style: {isBordered: 'is-bordered', isStriped: 'is-striped'},
	spacing: {isNarrow: 'is-narrow'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$tile = {
	container: 'tile',
	level: {isAncestor: 'is-ancestor', isParent: 'is-parent', isChild: 'is-child'},
	orientation: {isVertical: 'is-vertical'},
	width: {is1: 'is-1', is2: 'is-2', is3: 'is-3', is4: 'is-4', is5: 'is-5', is6: 'is-6', is7: 'is-7', is8: 'is-8', is9: 'is-9', is10: 'is-10', is11: 'is-11'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$columns = {
	container: 'columns',
	column: {
		container: 'column',
		width: {isOneQuarter: 'is-one-quarter', isOneThird: 'is-one-third', isHalf: 'is-half', isTwoThirds: 'is-two-thirds', isThreeQuarters: 'is-three-quarters', is1: 'is-1', is2: 'is-2', is3: 'is-3', is4: 'is-4', is5: 'is-5', is6: 'is-6', is7: 'is-7', is8: 'is-8', is9: 'is-9', is10: 'is-10', is11: 'is-11', isNarrow: 'is-narrow'},
		offset: {isOneQuarter: 'is-offset-one-quarter', isOneThird: 'is-offset-one-third', isHalf: 'is-offset-half', isTwoThirds: 'is-offset-two-thirds', isThreeQuarters: 'is-offset-three-quarters', is1: 'is-offset-1', is2: 'is-offset-2', is3: 'is-offset-3', is4: 'is-offset-4', is5: 'is-offset-5', is6: 'is-offset-6', is7: 'is-offset-7', is8: 'is-offset-8', is9: 'is-offset-9', is10: 'is-offset-10', is11: 'is-offset-11'}
	},
	alignment: {isCentered: 'is-centered', isVCentered: 'is-vcentered'},
	spacing: {isMultiline: 'is-multiline', isGapless: 'is-gapless', isGrid: 'is-grid'},
	display: {onMobile: 'is-mobile', onlyDesktop: 'is-desktop'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$feature = {
	container: 'container',
	sizing: {isFluid: 'is-fluid'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$level = {
	container: 'level',
	left: 'level-left',
	right: 'level-right',
	item: 'level-item',
	mobile: {isHorizontal: 'is-mobile'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$hero = {
	container: 'hero',
	head: 'hero-head',
	body: 'hero-body',
	foot: 'hero-foot',
	video: {
		container: 'hero-video',
		opacity: {isTransparent: 'is-transparent'}
	},
	buttons: {container: 'hero-buttons'},
	style: {isBold: 'is-bold'},
	size: {isMedium: 'is-medium', isLarge: 'is-large', isFullheight: 'is-fullheight'},
	color: {isPrimary: 'is-primary', isInfo: 'is-info', isSuccess: 'is-success', isWarning: 'is-warning', isDanger: 'is-danger', isWhite: 'is-white', isLight: 'is-light', isDark: 'is-dark', isBlack: 'is-black'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$nav = {
	container: 'nav',
	left: 'nav-left',
	center: 'nav-center',
	right: 'nav-right',
	toggle: {
		ui: 'nav-toggle',
		state: {isActive: 'is-active'}
	},
	menu: {
		container: 'nav-menu',
		state: {isActive: 'is-active'}
	},
	item: {
		container: 'nav-item',
		style: {isTab: 'is-tab'},
		state: {isActive: 'is-active'}
	},
	style: {hasShadow: 'has-shadow'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$footer = {container: 'footer'};
var _danielnarey$elm_bulma_classes$Internal_Classes$section = {
	container: 'section',
	spacing: {isMedium: 'is-medium', isLarge: 'is-large'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$number = {ui: 'number'};
var _danielnarey$elm_bulma_classes$Internal_Classes$tag = {
	ui: 'tag',
	size: {isMedium: 'is-medium', isLarge: 'is-large'},
	color: {isPrimary: 'is-primary', isInfo: 'is-info', isSuccess: 'is-success', isWarning: 'is-warning', isDanger: 'is-danger', isWhite: 'is-white', isLight: 'is-light', isDark: 'is-dark', isBlack: 'is-black'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$progress = {
	ui: 'progress',
	size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large'},
	color: {isPrimary: 'is-primary', isInfo: 'is-info', isSuccess: 'is-success', isWarning: 'is-warning', isDanger: 'is-danger', isWhite: 'is-white', isLight: 'is-light', isDark: 'is-dark', isBlack: 'is-black'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$notification = {
	ui: 'notification',
	color: {isPrimary: 'is-primary', isInfo: 'is-info', isSuccess: 'is-success', isWarning: 'is-warning', isDanger: 'is-danger', isWhite: 'is-white', isLight: 'is-light', isDark: 'is-dark', isBlack: 'is-black'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$help = {
	ui: 'help',
	color: {isPrimary: 'is-primary', isInfo: 'is-info', isSuccess: 'is-success', isWarning: 'is-warning', isDanger: 'is-danger', isWhite: 'is-white', isLight: 'is-light', isDark: 'is-dark', isBlack: 'is-black'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$label = {
	ui: 'label',
	size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$select = {
	ui: 'select',
	size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large', isFullwidth: 'is-fullwidth'},
	state: {isHovered: 'is-hovered', isFocused: 'is-focused', isActive: 'is-active', isLoading: 'is-loading', isDisabled: 'is-disabled'},
	addon: {isExpanded: 'is-expanded'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$radio = {ui: 'radio'};
var _danielnarey$elm_bulma_classes$Internal_Classes$checkbox = {ui: 'checkbox'};
var _danielnarey$elm_bulma_classes$Internal_Classes$textarea = {
	ui: 'textarea',
	display: {isInline: 'is-inline'},
	size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large', isFullwidth: 'is-fullwidth'},
	state: {isHovered: 'is-hovered', isFocused: 'is-focused', isActive: 'is-active', isLoading: 'is-loading'},
	color: {isPrimary: 'is-primary', isInfo: 'is-info', isSuccess: 'is-success', isWarning: 'is-warning', isDanger: 'is-danger', isWhite: 'is-white', isLight: 'is-light', isDark: 'is-dark', isBlack: 'is-black'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$input = {
	ui: 'input',
	display: {isInline: 'is-inline'},
	size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large', isFullwidth: 'is-fullwidth'},
	state: {isHovered: 'is-hovered', isFocused: 'is-focused', isActive: 'is-active', isLoading: 'is-loading'},
	color: {isPrimary: 'is-primary', isInfo: 'is-info', isSuccess: 'is-success', isWarning: 'is-warning', isDanger: 'is-danger', isWhite: 'is-white', isLight: 'is-light', isDark: 'is-dark', isBlack: 'is-black'},
	addon: {isExpanded: 'is-expanded'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$delete = {
	ui: 'delete',
	size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$button = {
	ui: 'button',
	style: {isLink: 'is-link', isOutlined: 'is-outlined', isInverted: 'is-inverted', isStatic: 'is-static'},
	size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large'},
	state: {isHovered: 'is-hovered', isFocused: 'is-focused', isActive: 'is-active', isLoading: 'is-loading'},
	color: {isPrimary: 'is-primary', isInfo: 'is-info', isSuccess: 'is-success', isWarning: 'is-warning', isDanger: 'is-danger', isWhite: 'is-white', isLight: 'is-light', isDark: 'is-dark', isBlack: 'is-black'},
	addon: {isExpanded: 'is-expanded'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$control = {
	container: 'control',
	hasIcons: {left: 'has-icons-left', right: 'has-icons-right'},
	state: {isLoading: 'is-loading'},
	sizing: {isExpanded: 'is-expanded'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$field = {
	container: 'field',
	label: 'field-label',
	body: 'field-body',
	isGrouped: {left: 'is-grouped', centered: 'is-grouped is-grouped-centered', right: 'is-grouped is-grouped-right'},
	hasAddons: {left: 'has-addons', centered: 'has-addons has-addons-centered', right: 'has-addons has-addons-right'},
	layout: {isHorizontal: 'is-horizontal'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$icon = {
	container: 'icon',
	size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$heading = {
	title: 'title',
	subtitle: 'subtitle',
	size: {is1: 'is-1', is2: 'is-2', is3: 'is-3', is4: 'is-4', is5: 'is-5', is6: 'is-6'},
	spacing: {isNormal: 'is-spaced'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$image = {
	container: 'image',
	size: {is16x16: 'is-16x16', is24x24: 'is-24x24', is32x32: 'is-32x32', is48x48: 'is-48x48', is64x64: 'is-64x64', is96x96: 'is-96x96', is128x128: 'is-128x128', isSquare: 'is-square', is1by1: 'is-1by1', is4by3: 'is-4by3', is3by2: 'is-3by2', is16by9: 'is-16by9', is2by1: 'is-2by1'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$content = {
	container: 'content',
	size: {isSmall: 'is-small', isMedium: 'is-medium', isLarge: 'is-large'}
};
var _danielnarey$elm_bulma_classes$Internal_Classes$properties = {
	$float: {isClearfix: 'is-clearfix', isPulledLeft: 'is-pulled-left', isPulledRight: 'is-pulled-right'},
	alignment: {hasTextCentered: 'has-text-centered', hasTextLeft: 'has-text-left', hasTextRight: 'has-text-right'},
	sizing: {isOverlay: 'is-overlay', isFullwidth: 'is-fullwidth', isMarginless: 'is-marginless', isPaddingless: 'is-paddingless'},
	display: {
		isBlock: {always: 'is-block-touch is-block-desktop', mobile: 'is-block-mobile', tablet: 'is-block-tablet', tabletOnly: 'is-block-tablet-only', touch: 'is-block-touch', desktop: 'is-block-desktop', desktopOnly: 'is-block-desktop-only', widescreen: 'is-block-widescreen'},
		isFlex: {always: 'is-flex-touch is-flex-desktop', mobile: 'is-flex-mobile', tablet: 'is-flex-tablet', tabletOnly: 'is-flex-tablet-only', touch: 'is-flex-touch', desktop: 'is-flex-desktop', desktopOnly: 'is-flex-desktop-only', widescreen: 'is-flex-widescreen'},
		isInline: {always: 'is-inline-touch is-inline-desktop', mobile: 'is-inline-mobile', tablet: 'is-inline-tablet', tabletOnly: 'is-inline-tablet-only', touch: 'is-inline-touch', desktop: 'is-inline-desktop', desktopOnly: 'is-inline-desktop-only', widescreen: 'is-inline-widescreen'},
		isInlineBlock: {always: 'is-inline-block-touch is-inline-block-desktop', mobile: 'is-inline-block-mobile', tablet: 'is-inline-block-tablet', tabletOnly: 'is-inline-block-tablet-only', touch: 'is-inline-block-touch', desktop: 'is-inline-block-desktop', desktopOnly: 'is-inline-block-desktop-only', widescreen: 'is-inline-block-widescreen'},
		isInlineFlex: {always: 'is-inline-flex-touch is-inline-flex-desktop', mobile: 'is-inline-flex-mobile', tablet: 'is-inline-flex-tablet', tabletOnly: 'is-inline-flex-tablet-only', touch: 'is-inline-flex-touch', desktop: 'is-inline-flex-desktop', desktopOnly: 'is-inline-flex-desktop-only', widescreen: 'is-inline-flex-widescreen'}
	},
	visibility: {
		isHidden: {always: 'is-hidden', mobile: 'is-hidden-mobile', tablet: 'is-hidden-tablet', tabletOnly: 'is-hidden-tablet-only', touch: 'is-hidden-touch', desktop: 'is-hidden-desktop', desktopOnly: 'is-hidden-desktop-only', widescreen: 'is-hidden-widescreen'}
	},
	interaction: {isUnselectable: 'is-unselectable'}
};

var _danielnarey$elm_bulma_classes$BulmaClasses$bulma = {properties: _danielnarey$elm_bulma_classes$Internal_Classes$properties, content: _danielnarey$elm_bulma_classes$Internal_Classes$content, image: _danielnarey$elm_bulma_classes$Internal_Classes$image, heading: _danielnarey$elm_bulma_classes$Internal_Classes$heading, icon: _danielnarey$elm_bulma_classes$Internal_Classes$icon, field: _danielnarey$elm_bulma_classes$Internal_Classes$field, control: _danielnarey$elm_bulma_classes$Internal_Classes$control, button: _danielnarey$elm_bulma_classes$Internal_Classes$button, $delete: _danielnarey$elm_bulma_classes$Internal_Classes$delete, input: _danielnarey$elm_bulma_classes$Internal_Classes$input, textarea: _danielnarey$elm_bulma_classes$Internal_Classes$textarea, checkbox: _danielnarey$elm_bulma_classes$Internal_Classes$checkbox, radio: _danielnarey$elm_bulma_classes$Internal_Classes$radio, select: _danielnarey$elm_bulma_classes$Internal_Classes$select, label: _danielnarey$elm_bulma_classes$Internal_Classes$label, help: _danielnarey$elm_bulma_classes$Internal_Classes$help, notification: _danielnarey$elm_bulma_classes$Internal_Classes$notification, progress: _danielnarey$elm_bulma_classes$Internal_Classes$progress, tag: _danielnarey$elm_bulma_classes$Internal_Classes$tag, number: _danielnarey$elm_bulma_classes$Internal_Classes$number, section: _danielnarey$elm_bulma_classes$Internal_Classes$section, footer: _danielnarey$elm_bulma_classes$Internal_Classes$footer, nav: _danielnarey$elm_bulma_classes$Internal_Classes$nav, hero: _danielnarey$elm_bulma_classes$Internal_Classes$hero, level: _danielnarey$elm_bulma_classes$Internal_Classes$level, feature: _danielnarey$elm_bulma_classes$Internal_Classes$feature, columns: _danielnarey$elm_bulma_classes$Internal_Classes$columns, tile: _danielnarey$elm_bulma_classes$Internal_Classes$tile, table: _danielnarey$elm_bulma_classes$Internal_Classes$table, menu: _danielnarey$elm_bulma_classes$Internal_Classes$menu, tabs: _danielnarey$elm_bulma_classes$Internal_Classes$tabs, panel: _danielnarey$elm_bulma_classes$Internal_Classes$panel, pagination: _danielnarey$elm_bulma_classes$Internal_Classes$pagination, box: _danielnarey$elm_bulma_classes$Internal_Classes$box, card: _danielnarey$elm_bulma_classes$Internal_Classes$card, media: _danielnarey$elm_bulma_classes$Internal_Classes$media, message: _danielnarey$elm_bulma_classes$Internal_Classes$message, modal: _danielnarey$elm_bulma_classes$Internal_Classes$modal};

//import Maybe, Native.List //

var _elm_lang$core$Native_Regex = function() {

function escape(str)
{
	return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
}
function caseInsensitive(re)
{
	return new RegExp(re.source, 'gi');
}
function regex(raw)
{
	return new RegExp(raw, 'g');
}

function contains(re, string)
{
	return string.match(re) !== null;
}

function find(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex === re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		out.push({
			match: result[0],
			submatches: _elm_lang$core$Native_List.fromArray(subs),
			index: result.index,
			number: number
		});
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

function replace(n, re, replacer, string)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch === undefined
				? _elm_lang$core$Maybe$Nothing
				: _elm_lang$core$Maybe$Just(submatch);
		}
		return replacer({
			match: match,
			submatches: _elm_lang$core$Native_List.fromArray(submatches),
			index: arguments[arguments.length - 2],
			number: count
		});
	}
	return string.replace(re, jsReplacer);
}

function split(n, re, str)
{
	n = n.ctor === 'All' ? Infinity : n._0;
	if (n === Infinity)
	{
		return _elm_lang$core$Native_List.fromArray(str.split(re));
	}
	var string = str;
	var result;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		if (!(result = re.exec(string))) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _elm_lang$core$Native_List.fromArray(out);
}

return {
	regex: regex,
	caseInsensitive: caseInsensitive,
	escape: escape,

	contains: F2(contains),
	find: F3(find),
	replace: F4(replace),
	split: F3(split)
};

}();

var _elm_lang$core$Regex$split = _elm_lang$core$Native_Regex.split;
var _elm_lang$core$Regex$replace = _elm_lang$core$Native_Regex.replace;
var _elm_lang$core$Regex$find = _elm_lang$core$Native_Regex.find;
var _elm_lang$core$Regex$contains = _elm_lang$core$Native_Regex.contains;
var _elm_lang$core$Regex$caseInsensitive = _elm_lang$core$Native_Regex.caseInsensitive;
var _elm_lang$core$Regex$regex = _elm_lang$core$Native_Regex.regex;
var _elm_lang$core$Regex$escape = _elm_lang$core$Native_Regex.escape;
var _elm_lang$core$Regex$Match = F4(
	function (a, b, c, d) {
		return {match: a, submatches: b, index: c, number: d};
	});
var _elm_lang$core$Regex$Regex = {ctor: 'Regex'};
var _elm_lang$core$Regex$AtMost = function (a) {
	return {ctor: 'AtMost', _0: a};
};
var _elm_lang$core$Regex$All = {ctor: 'All'};

var _elm_lang$core$Native_Bitwise = function() {

return {
	and: F2(function and(a, b) { return a & b; }),
	or: F2(function or(a, b) { return a | b; }),
	xor: F2(function xor(a, b) { return a ^ b; }),
	complement: function complement(a) { return ~a; },
	shiftLeftBy: F2(function(offset, a) { return a << offset; }),
	shiftRightBy: F2(function(offset, a) { return a >> offset; }),
	shiftRightZfBy: F2(function(offset, a) { return a >>> offset; })
};

}();

var _elm_lang$core$Bitwise$shiftRightZfBy = _elm_lang$core$Native_Bitwise.shiftRightZfBy;
var _elm_lang$core$Bitwise$shiftRightBy = _elm_lang$core$Native_Bitwise.shiftRightBy;
var _elm_lang$core$Bitwise$shiftLeftBy = _elm_lang$core$Native_Bitwise.shiftLeftBy;
var _elm_lang$core$Bitwise$complement = _elm_lang$core$Native_Bitwise.complement;
var _elm_lang$core$Bitwise$xor = _elm_lang$core$Native_Bitwise.xor;
var _elm_lang$core$Bitwise$or = _elm_lang$core$Native_Bitwise.or;
var _elm_lang$core$Bitwise$and = _elm_lang$core$Native_Bitwise.and;

var _elm_community$string_extra$String_Extra$accentRegex = function () {
	var matches = {
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: '[à-æ]', _1: 'a'},
		_1: {
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: '[À-Æ]', _1: 'A'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'ç', _1: 'c'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'Ç', _1: 'C'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: '[è-ë]', _1: 'e'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: '[È-Ë]', _1: 'E'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: '[ì-ï]', _1: 'i'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: '[Ì-Ï]', _1: 'I'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'ñ', _1: 'n'},
										_1: {
											ctor: '::',
											_0: {ctor: '_Tuple2', _0: 'Ñ', _1: 'N'},
											_1: {
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: '[ò-ö]', _1: 'o'},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: '[Ò-Ö]', _1: 'O'},
													_1: {
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: '[ù-ü]', _1: 'u'},
														_1: {
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: '[Ù-Ü]', _1: 'U'},
															_1: {
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'ý', _1: 'y'},
																_1: {
																	ctor: '::',
																	_0: {ctor: '_Tuple2', _0: 'ÿ', _1: 'y'},
																	_1: {
																		ctor: '::',
																		_0: {ctor: '_Tuple2', _0: 'Ý', _1: 'Y'},
																		_1: {ctor: '[]'}
																	}
																}
															}
														}
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	};
	return A2(
		_elm_lang$core$List$map,
		function (_p0) {
			var _p1 = _p0;
			return {
				ctor: '_Tuple2',
				_0: _elm_lang$core$Regex$regex(_p1._0),
				_1: _p1._1
			};
		},
		matches);
}();
var _elm_community$string_extra$String_Extra$removeAccents = function (string) {
	if (_elm_lang$core$String$isEmpty(string)) {
		return string;
	} else {
		var do_regex_to_remove_acents = function (_p2) {
			var _p3 = _p2;
			return A3(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_p3._0,
				function (_p4) {
					return _p3._1;
				});
		};
		return A3(_elm_lang$core$List$foldl, do_regex_to_remove_acents, string, _elm_community$string_extra$String_Extra$accentRegex);
	}
};
var _elm_community$string_extra$String_Extra$nonEmpty = function (string) {
	return _elm_lang$core$String$isEmpty(string) ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(string);
};
var _elm_community$string_extra$String_Extra$replacementCodePoint = 65533;
var _elm_community$string_extra$String_Extra$toCodePoints = function (string) {
	var allCodeUnits = A2(
		_elm_lang$core$List$map,
		_elm_lang$core$Char$toCode,
		_elm_lang$core$String$toList(string));
	var combineAndReverse = F2(
		function (codeUnits, accumulated) {
			combineAndReverse:
			while (true) {
				var _p5 = codeUnits;
				if (_p5.ctor === '[]') {
					return accumulated;
				} else {
					var _p9 = _p5._0;
					var _p8 = _p5._1;
					if ((_elm_lang$core$Native_Utils.cmp(_p9, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 55295) < 1)) {
						var _v3 = _p8,
							_v4 = {ctor: '::', _0: _p9, _1: accumulated};
						codeUnits = _v3;
						accumulated = _v4;
						continue combineAndReverse;
					} else {
						if ((_elm_lang$core$Native_Utils.cmp(_p9, 55296) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 56319) < 1)) {
							var _p6 = _p8;
							if (_p6.ctor === '[]') {
								return {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
							} else {
								var _p7 = _p6._0;
								if ((_elm_lang$core$Native_Utils.cmp(_p7, 56320) > -1) && (_elm_lang$core$Native_Utils.cmp(_p7, 57343) < 1)) {
									var codePoint = (65536 + ((_p9 - 55296) * 1024)) + (_p7 - 56320);
									var _v6 = _p6._1,
										_v7 = {ctor: '::', _0: codePoint, _1: accumulated};
									codeUnits = _v6;
									accumulated = _v7;
									continue combineAndReverse;
								} else {
									var _v8 = _p8,
										_v9 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
									codeUnits = _v8;
									accumulated = _v9;
									continue combineAndReverse;
								}
							}
						} else {
							if ((_elm_lang$core$Native_Utils.cmp(_p9, 57344) > -1) && (_elm_lang$core$Native_Utils.cmp(_p9, 65535) < 1)) {
								var _v10 = _p8,
									_v11 = {ctor: '::', _0: _p9, _1: accumulated};
								codeUnits = _v10;
								accumulated = _v11;
								continue combineAndReverse;
							} else {
								var _v12 = _p8,
									_v13 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
								codeUnits = _v12;
								accumulated = _v13;
								continue combineAndReverse;
							}
						}
					}
				}
			}
		});
	return _elm_lang$core$List$reverse(
		A2(
			combineAndReverse,
			allCodeUnits,
			{ctor: '[]'}));
};
var _elm_community$string_extra$String_Extra$fromCodePoints = function (allCodePoints) {
	var splitAndReverse = F2(
		function (codePoints, accumulated) {
			splitAndReverse:
			while (true) {
				var _p10 = codePoints;
				if (_p10.ctor === '[]') {
					return accumulated;
				} else {
					var _p12 = _p10._1;
					var _p11 = _p10._0;
					if ((_elm_lang$core$Native_Utils.cmp(_p11, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 55295) < 1)) {
						var _v15 = _p12,
							_v16 = {ctor: '::', _0: _p11, _1: accumulated};
						codePoints = _v15;
						accumulated = _v16;
						continue splitAndReverse;
					} else {
						if ((_elm_lang$core$Native_Utils.cmp(_p11, 65536) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 1114111) < 1)) {
							var subtracted = _p11 - 65536;
							var leading = (subtracted >> 10) + 55296;
							var trailing = (subtracted & 1023) + 56320;
							var _v17 = _p12,
								_v18 = {
								ctor: '::',
								_0: trailing,
								_1: {ctor: '::', _0: leading, _1: accumulated}
							};
							codePoints = _v17;
							accumulated = _v18;
							continue splitAndReverse;
						} else {
							if ((_elm_lang$core$Native_Utils.cmp(_p11, 57344) > -1) && (_elm_lang$core$Native_Utils.cmp(_p11, 65535) < 1)) {
								var _v19 = _p12,
									_v20 = {ctor: '::', _0: _p11, _1: accumulated};
								codePoints = _v19;
								accumulated = _v20;
								continue splitAndReverse;
							} else {
								var _v21 = _p12,
									_v22 = {ctor: '::', _0: _elm_community$string_extra$String_Extra$replacementCodePoint, _1: accumulated};
								codePoints = _v21;
								accumulated = _v22;
								continue splitAndReverse;
							}
						}
					}
				}
			}
		});
	var allCodeUnits = _elm_lang$core$List$reverse(
		A2(
			splitAndReverse,
			allCodePoints,
			{ctor: '[]'}));
	return _elm_lang$core$String$fromList(
		A2(_elm_lang$core$List$map, _elm_lang$core$Char$fromCode, allCodeUnits));
};
var _elm_community$string_extra$String_Extra$fromFloat = _elm_lang$core$Basics$toString;
var _elm_community$string_extra$String_Extra$fromInt = _elm_lang$core$Basics$toString;
var _elm_community$string_extra$String_Extra$leftOfBack = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				A2(_elm_lang$core$Basics$flip, _elm_lang$core$String$left, string),
				_elm_lang$core$List$head(
					_elm_lang$core$List$reverse(
						A2(_elm_lang$core$String$indexes, pattern, string)))));
	});
var _elm_community$string_extra$String_Extra$rightOfBack = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				function (_p13) {
					return A3(
						_elm_lang$core$Basics$flip,
						_elm_lang$core$String$dropLeft,
						string,
						A2(
							F2(
								function (x, y) {
									return x + y;
								}),
							_elm_lang$core$String$length(pattern),
							_p13));
				},
				_elm_lang$core$List$head(
					_elm_lang$core$List$reverse(
						A2(_elm_lang$core$String$indexes, pattern, string)))));
	});
var _elm_community$string_extra$String_Extra$firstResultHelp = F2(
	function ($default, list) {
		firstResultHelp:
		while (true) {
			var _p14 = list;
			if (_p14.ctor === '[]') {
				return $default;
			} else {
				if (_p14._0.ctor === 'Just') {
					return _p14._0._0;
				} else {
					var _v24 = $default,
						_v25 = _p14._1;
					$default = _v24;
					list = _v25;
					continue firstResultHelp;
				}
			}
		}
	});
var _elm_community$string_extra$String_Extra$firstResult = function (list) {
	return A2(_elm_community$string_extra$String_Extra$firstResultHelp, '', list);
};
var _elm_community$string_extra$String_Extra$leftOf = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$String$join,
			'',
			A2(
				_elm_lang$core$List$map,
				function (_p15) {
					return _elm_community$string_extra$String_Extra$firstResult(
						function (_) {
							return _.submatches;
						}(_p15));
				},
				A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_elm_lang$core$Regex$regex(
						A2(
							_elm_lang$core$Basics_ops['++'],
							'^(.*?)',
							_elm_lang$core$Regex$escape(pattern))),
					string)));
	});
var _elm_community$string_extra$String_Extra$rightOf = F2(
	function (pattern, string) {
		return A2(
			_elm_lang$core$String$join,
			'',
			A2(
				_elm_lang$core$List$map,
				function (_p16) {
					return _elm_community$string_extra$String_Extra$firstResult(
						function (_) {
							return _.submatches;
						}(_p16));
				},
				A3(
					_elm_lang$core$Regex$find,
					_elm_lang$core$Regex$AtMost(1),
					_elm_lang$core$Regex$regex(
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Regex$escape(pattern),
							'(.*)$')),
					string)));
	});
var _elm_community$string_extra$String_Extra$pluralize = F3(
	function (singular, plural, count) {
		return _elm_lang$core$Native_Utils.eq(count, 1) ? A2(_elm_lang$core$Basics_ops['++'], '1 ', singular) : A2(
			_elm_lang$core$Basics_ops['++'],
			_elm_lang$core$Basics$toString(count),
			A2(_elm_lang$core$Basics_ops['++'], ' ', plural));
	});
var _elm_community$string_extra$String_Extra$stripTags = function (string) {
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('<\\/?[^>]+>'),
		_elm_lang$core$Basics$always(''),
		string);
};
var _elm_community$string_extra$String_Extra$toSentenceHelper = F3(
	function (lastPart, sentence, list) {
		toSentenceHelper:
		while (true) {
			var _p17 = list;
			if (_p17.ctor === '[]') {
				return sentence;
			} else {
				if (_p17._1.ctor === '[]') {
					return A2(
						_elm_lang$core$Basics_ops['++'],
						sentence,
						A2(_elm_lang$core$Basics_ops['++'], lastPart, _p17._0));
				} else {
					var _v27 = lastPart,
						_v28 = A2(
						_elm_lang$core$Basics_ops['++'],
						sentence,
						A2(_elm_lang$core$Basics_ops['++'], ', ', _p17._0)),
						_v29 = _p17._1;
					lastPart = _v27;
					sentence = _v28;
					list = _v29;
					continue toSentenceHelper;
				}
			}
		}
	});
var _elm_community$string_extra$String_Extra$toSentenceBaseCase = function (list) {
	var _p18 = list;
	_v30_2:
	do {
		if (_p18.ctor === '::') {
			if (_p18._1.ctor === '[]') {
				return _p18._0;
			} else {
				if (_p18._1._1.ctor === '[]') {
					return A2(
						_elm_lang$core$Basics_ops['++'],
						_p18._0,
						A2(_elm_lang$core$Basics_ops['++'], ' and ', _p18._1._0));
				} else {
					break _v30_2;
				}
			}
		} else {
			break _v30_2;
		}
	} while(false);
	return '';
};
var _elm_community$string_extra$String_Extra$toSentenceOxford = function (list) {
	var _p19 = list;
	if (((_p19.ctor === '::') && (_p19._1.ctor === '::')) && (_p19._1._1.ctor === '::')) {
		return A3(
			_elm_community$string_extra$String_Extra$toSentenceHelper,
			', and ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_p19._0,
				A2(_elm_lang$core$Basics_ops['++'], ', ', _p19._1._0)),
			{ctor: '::', _0: _p19._1._1._0, _1: _p19._1._1._1});
	} else {
		return _elm_community$string_extra$String_Extra$toSentenceBaseCase(list);
	}
};
var _elm_community$string_extra$String_Extra$toSentence = function (list) {
	var _p20 = list;
	if (((_p20.ctor === '::') && (_p20._1.ctor === '::')) && (_p20._1._1.ctor === '::')) {
		return A3(
			_elm_community$string_extra$String_Extra$toSentenceHelper,
			' and ',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_p20._0,
				A2(_elm_lang$core$Basics_ops['++'], ', ', _p20._1._0)),
			{ctor: '::', _0: _p20._1._1._0, _1: _p20._1._1._1});
	} else {
		return _elm_community$string_extra$String_Extra$toSentenceBaseCase(list);
	}
};
var _elm_community$string_extra$String_Extra$ellipsisWith = F3(
	function (howLong, append, string) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$String$length(string),
			howLong) < 1) ? string : A2(
			_elm_lang$core$Basics_ops['++'],
			A2(
				_elm_lang$core$String$left,
				howLong - _elm_lang$core$String$length(append),
				string),
			append);
	});
var _elm_community$string_extra$String_Extra$ellipsis = F2(
	function (howLong, string) {
		return A3(_elm_community$string_extra$String_Extra$ellipsisWith, howLong, '...', string);
	});
var _elm_community$string_extra$String_Extra$countOccurrences = F2(
	function (needle, haystack) {
		return (_elm_lang$core$Native_Utils.eq(
			_elm_lang$core$String$length(needle),
			0) || _elm_lang$core$Native_Utils.eq(
			_elm_lang$core$String$length(haystack),
			0)) ? 0 : _elm_lang$core$List$length(
			A2(_elm_lang$core$String$indexes, needle, haystack));
	});
var _elm_community$string_extra$String_Extra$unindent = function (multilineSting) {
	var isNotWhitespace = function ($char) {
		return (!_elm_lang$core$Native_Utils.eq(
			$char,
			_elm_lang$core$Native_Utils.chr(' '))) && (!_elm_lang$core$Native_Utils.eq(
			$char,
			_elm_lang$core$Native_Utils.chr('\t')));
	};
	var countLeadingWhitespace = F2(
		function (count, line) {
			countLeadingWhitespace:
			while (true) {
				var _p21 = _elm_lang$core$String$uncons(line);
				if (_p21.ctor === 'Nothing') {
					return count;
				} else {
					var _p23 = _p21._0._1;
					var _p22 = _p21._0._0;
					switch (_p22.valueOf()) {
						case ' ':
							var _v35 = count + 1,
								_v36 = _p23;
							count = _v35;
							line = _v36;
							continue countLeadingWhitespace;
						case '\t':
							var _v37 = count + 1,
								_v38 = _p23;
							count = _v37;
							line = _v38;
							continue countLeadingWhitespace;
						default:
							return count;
					}
				}
			}
		});
	var lines = _elm_lang$core$String$lines(multilineSting);
	var minLead = A2(
		_elm_lang$core$Maybe$withDefault,
		0,
		_elm_lang$core$List$minimum(
			A2(
				_elm_lang$core$List$map,
				countLeadingWhitespace(0),
				A2(
					_elm_lang$core$List$filter,
					_elm_lang$core$String$any(isNotWhitespace),
					lines))));
	return A2(
		_elm_lang$core$String$join,
		'\n',
		A2(
			_elm_lang$core$List$map,
			_elm_lang$core$String$dropLeft(minLead),
			lines));
};
var _elm_community$string_extra$String_Extra$dasherize = function (string) {
	return _elm_lang$core$String$toLower(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('[_-\\s]+'),
			_elm_lang$core$Basics$always('-'),
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([A-Z])'),
				function (_p24) {
					return A2(
						_elm_lang$core$String$append,
						'-',
						function (_) {
							return _.match;
						}(_p24));
				},
				_elm_lang$core$String$trim(string))));
};
var _elm_community$string_extra$String_Extra$underscored = function (string) {
	return _elm_lang$core$String$toLower(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('[_-\\s]+'),
			_elm_lang$core$Basics$always('_'),
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([a-z\\d])([A-Z]+)'),
				function (_p25) {
					return A2(
						_elm_lang$core$String$join,
						'_',
						A2(
							_elm_lang$core$List$filterMap,
							_elm_lang$core$Basics$identity,
							function (_) {
								return _.submatches;
							}(_p25)));
				},
				_elm_lang$core$String$trim(string))));
};
var _elm_community$string_extra$String_Extra$unsurround = F2(
	function (wrap, string) {
		if (A2(_elm_lang$core$String$startsWith, wrap, string) && A2(_elm_lang$core$String$endsWith, wrap, string)) {
			var length = _elm_lang$core$String$length(wrap);
			return A2(
				_elm_lang$core$String$dropRight,
				length,
				A2(_elm_lang$core$String$dropLeft, length, string));
		} else {
			return string;
		}
	});
var _elm_community$string_extra$String_Extra$unquote = function (string) {
	return A2(_elm_community$string_extra$String_Extra$unsurround, '\"', string);
};
var _elm_community$string_extra$String_Extra$surround = F2(
	function (wrap, string) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			wrap,
			A2(_elm_lang$core$Basics_ops['++'], string, wrap));
	});
var _elm_community$string_extra$String_Extra$quote = function (string) {
	return A2(_elm_community$string_extra$String_Extra$surround, '\"', string);
};
var _elm_community$string_extra$String_Extra$camelize = function (string) {
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('[-_\\s]+(.)?'),
		function (_p26) {
			var _p27 = _p26;
			var _p28 = _p27.submatches;
			if ((_p28.ctor === '::') && (_p28._0.ctor === 'Just')) {
				return _elm_lang$core$String$toUpper(_p28._0._0);
			} else {
				return '';
			}
		},
		_elm_lang$core$String$trim(string));
};
var _elm_community$string_extra$String_Extra$isBlank = function (string) {
	return A2(
		_elm_lang$core$Regex$contains,
		_elm_lang$core$Regex$regex('^\\s*$'),
		string);
};
var _elm_community$string_extra$String_Extra$clean = function (string) {
	return _elm_lang$core$String$trim(
		A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex('\\s\\s+'),
			_elm_lang$core$Basics$always(' '),
			string));
};
var _elm_community$string_extra$String_Extra$softBreakRegexp = function (width) {
	return _elm_lang$core$Regex$regex(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'.{1,',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(width),
				'}(\\s+|$)|\\S+?(\\s+|$)')));
};
var _elm_community$string_extra$String_Extra$softEllipsis = F2(
	function (howLong, string) {
		return (_elm_lang$core$Native_Utils.cmp(
			_elm_lang$core$String$length(string),
			howLong) < 1) ? string : A3(
			_elm_lang$core$Basics$flip,
			_elm_lang$core$String$append,
			'...',
			A4(
				_elm_lang$core$Regex$replace,
				_elm_lang$core$Regex$All,
				_elm_lang$core$Regex$regex('([\\.,;:\\s])+$'),
				_elm_lang$core$Basics$always(''),
				A2(
					_elm_lang$core$String$join,
					'',
					A2(
						_elm_lang$core$List$map,
						function (_) {
							return _.match;
						},
						A3(
							_elm_lang$core$Regex$find,
							_elm_lang$core$Regex$AtMost(1),
							_elm_community$string_extra$String_Extra$softBreakRegexp(howLong),
							string)))));
	});
var _elm_community$string_extra$String_Extra$softBreak = F2(
	function (width, string) {
		return (_elm_lang$core$Native_Utils.cmp(width, 0) < 1) ? {ctor: '[]'} : A2(
			_elm_lang$core$List$map,
			function (_) {
				return _.match;
			},
			A3(
				_elm_lang$core$Regex$find,
				_elm_lang$core$Regex$All,
				_elm_community$string_extra$String_Extra$softBreakRegexp(width),
				string));
	});
var _elm_community$string_extra$String_Extra$softWrapWith = F3(
	function (width, separator, string) {
		return A2(
			_elm_lang$core$String$join,
			separator,
			A2(_elm_community$string_extra$String_Extra$softBreak, width, string));
	});
var _elm_community$string_extra$String_Extra$softWrap = F2(
	function (width, string) {
		return A3(_elm_community$string_extra$String_Extra$softWrapWith, width, '\n', string);
	});
var _elm_community$string_extra$String_Extra$breaker = F3(
	function (width, string, acc) {
		breaker:
		while (true) {
			var _p29 = string;
			if (_p29 === '') {
				return _elm_lang$core$List$reverse(acc);
			} else {
				var _v42 = width,
					_v43 = A2(_elm_lang$core$String$dropLeft, width, string),
					_v44 = {
					ctor: '::',
					_0: A3(_elm_lang$core$String$slice, 0, width, string),
					_1: acc
				};
				width = _v42;
				string = _v43;
				acc = _v44;
				continue breaker;
			}
		}
	});
var _elm_community$string_extra$String_Extra$break = F2(
	function (width, string) {
		return (_elm_lang$core$Native_Utils.eq(width, 0) || _elm_lang$core$Native_Utils.eq(string, '')) ? {
			ctor: '::',
			_0: string,
			_1: {ctor: '[]'}
		} : A3(
			_elm_community$string_extra$String_Extra$breaker,
			width,
			string,
			{ctor: '[]'});
	});
var _elm_community$string_extra$String_Extra$wrapWith = F3(
	function (width, separator, string) {
		return A2(
			_elm_lang$core$String$join,
			separator,
			A2(_elm_community$string_extra$String_Extra$break, width, string));
	});
var _elm_community$string_extra$String_Extra$wrap = F2(
	function (width, string) {
		return A3(_elm_community$string_extra$String_Extra$wrapWith, width, '\n', string);
	});
var _elm_community$string_extra$String_Extra$replaceSlice = F4(
	function (substitution, start, end, string) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			A3(_elm_lang$core$String$slice, 0, start, string),
			A2(
				_elm_lang$core$Basics_ops['++'],
				substitution,
				A3(
					_elm_lang$core$String$slice,
					end,
					_elm_lang$core$String$length(string),
					string)));
	});
var _elm_community$string_extra$String_Extra$insertAt = F3(
	function (insert, pos, string) {
		return A4(_elm_community$string_extra$String_Extra$replaceSlice, insert, pos, pos, string);
	});
var _elm_community$string_extra$String_Extra$replace = F3(
	function (search, substitution, string) {
		return A4(
			_elm_lang$core$Regex$replace,
			_elm_lang$core$Regex$All,
			_elm_lang$core$Regex$regex(
				_elm_lang$core$Regex$escape(search)),
			function (_p30) {
				return substitution;
			},
			string);
	});
var _elm_community$string_extra$String_Extra$changeCase = F2(
	function (mutator, word) {
		return A2(
			_elm_lang$core$Maybe$withDefault,
			'',
			A2(
				_elm_lang$core$Maybe$map,
				function (_p31) {
					var _p32 = _p31;
					return A2(
						_elm_lang$core$String$cons,
						mutator(_p32._0),
						_p32._1);
				},
				_elm_lang$core$String$uncons(word)));
	});
var _elm_community$string_extra$String_Extra$toSentenceCase = function (word) {
	return A2(_elm_community$string_extra$String_Extra$changeCase, _elm_lang$core$Char$toUpper, word);
};
var _elm_community$string_extra$String_Extra$toTitleCase = function (ws) {
	var uppercaseMatch = A3(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('\\w+'),
		function (_p33) {
			return _elm_community$string_extra$String_Extra$toSentenceCase(
				function (_) {
					return _.match;
				}(_p33));
		});
	return A4(
		_elm_lang$core$Regex$replace,
		_elm_lang$core$Regex$All,
		_elm_lang$core$Regex$regex('^([a-z])|\\s+([a-z])'),
		function (_p34) {
			return uppercaseMatch(
				function (_) {
					return _.match;
				}(_p34));
		},
		ws);
};
var _elm_community$string_extra$String_Extra$classify = function (string) {
	return _elm_community$string_extra$String_Extra$toSentenceCase(
		A3(
			_elm_community$string_extra$String_Extra$replace,
			' ',
			'',
			_elm_community$string_extra$String_Extra$camelize(
				A4(
					_elm_lang$core$Regex$replace,
					_elm_lang$core$Regex$All,
					_elm_lang$core$Regex$regex('[\\W_]'),
					_elm_lang$core$Basics$always(' '),
					string))));
};
var _elm_community$string_extra$String_Extra$humanize = function (string) {
	return _elm_community$string_extra$String_Extra$toSentenceCase(
		_elm_lang$core$String$toLower(
			_elm_lang$core$String$trim(
				A4(
					_elm_lang$core$Regex$replace,
					_elm_lang$core$Regex$All,
					_elm_lang$core$Regex$regex('_id$|[-_\\s]+'),
					_elm_lang$core$Basics$always(' '),
					A4(
						_elm_lang$core$Regex$replace,
						_elm_lang$core$Regex$All,
						_elm_lang$core$Regex$regex('[A-Z]'),
						function (_p35) {
							return A2(
								_elm_lang$core$String$append,
								'-',
								function (_) {
									return _.match;
								}(_p35));
						},
						string)))));
};
var _elm_community$string_extra$String_Extra$decapitalize = function (word) {
	return A2(_elm_community$string_extra$String_Extra$changeCase, _elm_lang$core$Char$toLower, word);
};

//import Result //

var _elm_lang$core$Native_Date = function() {

function fromString(str)
{
	var date = new Date(str);
	return isNaN(date.getTime())
		? _elm_lang$core$Result$Err('Unable to parse \'' + str + '\' as a date. Dates must be in the ISO 8601 format.')
		: _elm_lang$core$Result$Ok(date);
}

var dayTable = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
var monthTable =
	['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
	 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];


return {
	fromString: fromString,
	year: function(d) { return d.getFullYear(); },
	month: function(d) { return { ctor: monthTable[d.getMonth()] }; },
	day: function(d) { return d.getDate(); },
	hour: function(d) { return d.getHours(); },
	minute: function(d) { return d.getMinutes(); },
	second: function(d) { return d.getSeconds(); },
	millisecond: function(d) { return d.getMilliseconds(); },
	toTime: function(d) { return d.getTime(); },
	fromTime: function(t) { return new Date(t); },
	dayOfWeek: function(d) { return { ctor: dayTable[d.getDay()] }; }
};

}();
var _elm_lang$core$Task$onError = _elm_lang$core$Native_Scheduler.onError;
var _elm_lang$core$Task$andThen = _elm_lang$core$Native_Scheduler.andThen;
var _elm_lang$core$Task$spawnCmd = F2(
	function (router, _p0) {
		var _p1 = _p0;
		return _elm_lang$core$Native_Scheduler.spawn(
			A2(
				_elm_lang$core$Task$andThen,
				_elm_lang$core$Platform$sendToApp(router),
				_p1._0));
	});
var _elm_lang$core$Task$fail = _elm_lang$core$Native_Scheduler.fail;
var _elm_lang$core$Task$mapError = F2(
	function (convert, task) {
		return A2(
			_elm_lang$core$Task$onError,
			function (_p2) {
				return _elm_lang$core$Task$fail(
					convert(_p2));
			},
			task);
	});
var _elm_lang$core$Task$succeed = _elm_lang$core$Native_Scheduler.succeed;
var _elm_lang$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return _elm_lang$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var _elm_lang$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return _elm_lang$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map3 = F4(
	function (func, taskA, taskB, taskC) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return _elm_lang$core$Task$succeed(
									A3(func, a, b, c));
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map4 = F5(
	function (func, taskA, taskB, taskC, taskD) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return _elm_lang$core$Task$succeed(
											A4(func, a, b, c, d));
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$map5 = F6(
	function (func, taskA, taskB, taskC, taskD, taskE) {
		return A2(
			_elm_lang$core$Task$andThen,
			function (a) {
				return A2(
					_elm_lang$core$Task$andThen,
					function (b) {
						return A2(
							_elm_lang$core$Task$andThen,
							function (c) {
								return A2(
									_elm_lang$core$Task$andThen,
									function (d) {
										return A2(
											_elm_lang$core$Task$andThen,
											function (e) {
												return _elm_lang$core$Task$succeed(
													A5(func, a, b, c, d, e));
											},
											taskE);
									},
									taskD);
							},
							taskC);
					},
					taskB);
			},
			taskA);
	});
var _elm_lang$core$Task$sequence = function (tasks) {
	var _p3 = tasks;
	if (_p3.ctor === '[]') {
		return _elm_lang$core$Task$succeed(
			{ctor: '[]'});
	} else {
		return A3(
			_elm_lang$core$Task$map2,
			F2(
				function (x, y) {
					return {ctor: '::', _0: x, _1: y};
				}),
			_p3._0,
			_elm_lang$core$Task$sequence(_p3._1));
	}
};
var _elm_lang$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			_elm_lang$core$Task$map,
			function (_p4) {
				return {ctor: '_Tuple0'};
			},
			_elm_lang$core$Task$sequence(
				A2(
					_elm_lang$core$List$map,
					_elm_lang$core$Task$spawnCmd(router),
					commands)));
	});
var _elm_lang$core$Task$init = _elm_lang$core$Task$succeed(
	{ctor: '_Tuple0'});
var _elm_lang$core$Task$onSelfMsg = F3(
	function (_p7, _p6, _p5) {
		return _elm_lang$core$Task$succeed(
			{ctor: '_Tuple0'});
	});
var _elm_lang$core$Task$command = _elm_lang$core$Native_Platform.leaf('Task');
var _elm_lang$core$Task$Perform = function (a) {
	return {ctor: 'Perform', _0: a};
};
var _elm_lang$core$Task$perform = F2(
	function (toMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(_elm_lang$core$Task$map, toMessage, task)));
	});
var _elm_lang$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return _elm_lang$core$Task$command(
			_elm_lang$core$Task$Perform(
				A2(
					_elm_lang$core$Task$onError,
					function (_p8) {
						return _elm_lang$core$Task$succeed(
							resultToMessage(
								_elm_lang$core$Result$Err(_p8)));
					},
					A2(
						_elm_lang$core$Task$andThen,
						function (_p9) {
							return _elm_lang$core$Task$succeed(
								resultToMessage(
									_elm_lang$core$Result$Ok(_p9)));
						},
						task))));
	});
var _elm_lang$core$Task$cmdMap = F2(
	function (tagger, _p10) {
		var _p11 = _p10;
		return _elm_lang$core$Task$Perform(
			A2(_elm_lang$core$Task$map, tagger, _p11._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Task'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Task$init, onEffects: _elm_lang$core$Task$onEffects, onSelfMsg: _elm_lang$core$Task$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Task$cmdMap};

//import Native.Scheduler //

var _elm_lang$core$Native_Time = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(Date.now()));
});

function setInterval_(interval, task)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var id = setInterval(function() {
			_elm_lang$core$Native_Scheduler.rawSpawn(task);
		}, interval);

		return function() { clearInterval(id); };
	});
}

return {
	now: now,
	setInterval_: F2(setInterval_)
};

}();
var _elm_lang$core$Time$setInterval = _elm_lang$core$Native_Time.setInterval_;
var _elm_lang$core$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		var _p0 = intervals;
		if (_p0.ctor === '[]') {
			return _elm_lang$core$Task$succeed(processes);
		} else {
			var _p1 = _p0._0;
			var spawnRest = function (id) {
				return A3(
					_elm_lang$core$Time$spawnHelp,
					router,
					_p0._1,
					A3(_elm_lang$core$Dict$insert, _p1, id, processes));
			};
			var spawnTimer = _elm_lang$core$Native_Scheduler.spawn(
				A2(
					_elm_lang$core$Time$setInterval,
					_p1,
					A2(_elm_lang$core$Platform$sendToSelf, router, _p1)));
			return A2(_elm_lang$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var _elm_lang$core$Time$addMySub = F2(
	function (_p2, state) {
		var _p3 = _p2;
		var _p6 = _p3._1;
		var _p5 = _p3._0;
		var _p4 = A2(_elm_lang$core$Dict$get, _p5, state);
		if (_p4.ctor === 'Nothing') {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{
					ctor: '::',
					_0: _p6,
					_1: {ctor: '[]'}
				},
				state);
		} else {
			return A3(
				_elm_lang$core$Dict$insert,
				_p5,
				{ctor: '::', _0: _p6, _1: _p4._0},
				state);
		}
	});
var _elm_lang$core$Time$inMilliseconds = function (t) {
	return t;
};
var _elm_lang$core$Time$millisecond = 1;
var _elm_lang$core$Time$second = 1000 * _elm_lang$core$Time$millisecond;
var _elm_lang$core$Time$minute = 60 * _elm_lang$core$Time$second;
var _elm_lang$core$Time$hour = 60 * _elm_lang$core$Time$minute;
var _elm_lang$core$Time$inHours = function (t) {
	return t / _elm_lang$core$Time$hour;
};
var _elm_lang$core$Time$inMinutes = function (t) {
	return t / _elm_lang$core$Time$minute;
};
var _elm_lang$core$Time$inSeconds = function (t) {
	return t / _elm_lang$core$Time$second;
};
var _elm_lang$core$Time$now = _elm_lang$core$Native_Time.now;
var _elm_lang$core$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _p7 = A2(_elm_lang$core$Dict$get, interval, state.taggers);
		if (_p7.ctor === 'Nothing') {
			return _elm_lang$core$Task$succeed(state);
		} else {
			var tellTaggers = function (time) {
				return _elm_lang$core$Task$sequence(
					A2(
						_elm_lang$core$List$map,
						function (tagger) {
							return A2(
								_elm_lang$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						_p7._0));
			};
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p8) {
					return _elm_lang$core$Task$succeed(state);
				},
				A2(_elm_lang$core$Task$andThen, tellTaggers, _elm_lang$core$Time$now));
		}
	});
var _elm_lang$core$Time$subscription = _elm_lang$core$Native_Platform.leaf('Time');
var _elm_lang$core$Time$State = F2(
	function (a, b) {
		return {taggers: a, processes: b};
	});
var _elm_lang$core$Time$init = _elm_lang$core$Task$succeed(
	A2(_elm_lang$core$Time$State, _elm_lang$core$Dict$empty, _elm_lang$core$Dict$empty));
var _elm_lang$core$Time$onEffects = F3(
	function (router, subs, _p9) {
		var _p10 = _p9;
		var rightStep = F3(
			function (_p12, id, _p11) {
				var _p13 = _p11;
				return {
					ctor: '_Tuple3',
					_0: _p13._0,
					_1: _p13._1,
					_2: A2(
						_elm_lang$core$Task$andThen,
						function (_p14) {
							return _p13._2;
						},
						_elm_lang$core$Native_Scheduler.kill(id))
				};
			});
		var bothStep = F4(
			function (interval, taggers, id, _p15) {
				var _p16 = _p15;
				return {
					ctor: '_Tuple3',
					_0: _p16._0,
					_1: A3(_elm_lang$core$Dict$insert, interval, id, _p16._1),
					_2: _p16._2
				};
			});
		var leftStep = F3(
			function (interval, taggers, _p17) {
				var _p18 = _p17;
				return {
					ctor: '_Tuple3',
					_0: {ctor: '::', _0: interval, _1: _p18._0},
					_1: _p18._1,
					_2: _p18._2
				};
			});
		var newTaggers = A3(_elm_lang$core$List$foldl, _elm_lang$core$Time$addMySub, _elm_lang$core$Dict$empty, subs);
		var _p19 = A6(
			_elm_lang$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			_p10.processes,
			{
				ctor: '_Tuple3',
				_0: {ctor: '[]'},
				_1: _elm_lang$core$Dict$empty,
				_2: _elm_lang$core$Task$succeed(
					{ctor: '_Tuple0'})
			});
		var spawnList = _p19._0;
		var existingDict = _p19._1;
		var killTask = _p19._2;
		return A2(
			_elm_lang$core$Task$andThen,
			function (newProcesses) {
				return _elm_lang$core$Task$succeed(
					A2(_elm_lang$core$Time$State, newTaggers, newProcesses));
			},
			A2(
				_elm_lang$core$Task$andThen,
				function (_p20) {
					return A3(_elm_lang$core$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var _elm_lang$core$Time$Every = F2(
	function (a, b) {
		return {ctor: 'Every', _0: a, _1: b};
	});
var _elm_lang$core$Time$every = F2(
	function (interval, tagger) {
		return _elm_lang$core$Time$subscription(
			A2(_elm_lang$core$Time$Every, interval, tagger));
	});
var _elm_lang$core$Time$subMap = F2(
	function (f, _p21) {
		var _p22 = _p21;
		return A2(
			_elm_lang$core$Time$Every,
			_p22._0,
			function (_p23) {
				return f(
					_p22._1(_p23));
			});
	});
_elm_lang$core$Native_Platform.effectManagers['Time'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Time$init, onEffects: _elm_lang$core$Time$onEffects, onSelfMsg: _elm_lang$core$Time$onSelfMsg, tag: 'sub', subMap: _elm_lang$core$Time$subMap};

var _elm_lang$core$Date$millisecond = _elm_lang$core$Native_Date.millisecond;
var _elm_lang$core$Date$second = _elm_lang$core$Native_Date.second;
var _elm_lang$core$Date$minute = _elm_lang$core$Native_Date.minute;
var _elm_lang$core$Date$hour = _elm_lang$core$Native_Date.hour;
var _elm_lang$core$Date$dayOfWeek = _elm_lang$core$Native_Date.dayOfWeek;
var _elm_lang$core$Date$day = _elm_lang$core$Native_Date.day;
var _elm_lang$core$Date$month = _elm_lang$core$Native_Date.month;
var _elm_lang$core$Date$year = _elm_lang$core$Native_Date.year;
var _elm_lang$core$Date$fromTime = _elm_lang$core$Native_Date.fromTime;
var _elm_lang$core$Date$toTime = _elm_lang$core$Native_Date.toTime;
var _elm_lang$core$Date$fromString = _elm_lang$core$Native_Date.fromString;
var _elm_lang$core$Date$now = A2(_elm_lang$core$Task$map, _elm_lang$core$Date$fromTime, _elm_lang$core$Time$now);
var _elm_lang$core$Date$Date = {ctor: 'Date'};
var _elm_lang$core$Date$Sun = {ctor: 'Sun'};
var _elm_lang$core$Date$Sat = {ctor: 'Sat'};
var _elm_lang$core$Date$Fri = {ctor: 'Fri'};
var _elm_lang$core$Date$Thu = {ctor: 'Thu'};
var _elm_lang$core$Date$Wed = {ctor: 'Wed'};
var _elm_lang$core$Date$Tue = {ctor: 'Tue'};
var _elm_lang$core$Date$Mon = {ctor: 'Mon'};
var _elm_lang$core$Date$Dec = {ctor: 'Dec'};
var _elm_lang$core$Date$Nov = {ctor: 'Nov'};
var _elm_lang$core$Date$Oct = {ctor: 'Oct'};
var _elm_lang$core$Date$Sep = {ctor: 'Sep'};
var _elm_lang$core$Date$Aug = {ctor: 'Aug'};
var _elm_lang$core$Date$Jul = {ctor: 'Jul'};
var _elm_lang$core$Date$Jun = {ctor: 'Jun'};
var _elm_lang$core$Date$May = {ctor: 'May'};
var _elm_lang$core$Date$Apr = {ctor: 'Apr'};
var _elm_lang$core$Date$Mar = {ctor: 'Mar'};
var _elm_lang$core$Date$Feb = {ctor: 'Feb'};
var _elm_lang$core$Date$Jan = {ctor: 'Jan'};

var _elm_lang$core$Process$kill = _elm_lang$core$Native_Scheduler.kill;
var _elm_lang$core$Process$sleep = _elm_lang$core$Native_Scheduler.sleep;
var _elm_lang$core$Process$spawn = _elm_lang$core$Native_Scheduler.spawn;

var _elm_lang$core$Random$onSelfMsg = F3(
	function (_p1, _p0, seed) {
		return _elm_lang$core$Task$succeed(seed);
	});
var _elm_lang$core$Random$magicNum8 = 2147483562;
var _elm_lang$core$Random$range = function (_p2) {
	return {ctor: '_Tuple2', _0: 0, _1: _elm_lang$core$Random$magicNum8};
};
var _elm_lang$core$Random$magicNum7 = 2147483399;
var _elm_lang$core$Random$magicNum6 = 2147483563;
var _elm_lang$core$Random$magicNum5 = 3791;
var _elm_lang$core$Random$magicNum4 = 40692;
var _elm_lang$core$Random$magicNum3 = 52774;
var _elm_lang$core$Random$magicNum2 = 12211;
var _elm_lang$core$Random$magicNum1 = 53668;
var _elm_lang$core$Random$magicNum0 = 40014;
var _elm_lang$core$Random$step = F2(
	function (_p3, seed) {
		var _p4 = _p3;
		return _p4._0(seed);
	});
var _elm_lang$core$Random$onEffects = F3(
	function (router, commands, seed) {
		var _p5 = commands;
		if (_p5.ctor === '[]') {
			return _elm_lang$core$Task$succeed(seed);
		} else {
			var _p6 = A2(_elm_lang$core$Random$step, _p5._0._0, seed);
			var value = _p6._0;
			var newSeed = _p6._1;
			return A2(
				_elm_lang$core$Task$andThen,
				function (_p7) {
					return A3(_elm_lang$core$Random$onEffects, router, _p5._1, newSeed);
				},
				A2(_elm_lang$core$Platform$sendToApp, router, value));
		}
	});
var _elm_lang$core$Random$listHelp = F4(
	function (list, n, generate, seed) {
		listHelp:
		while (true) {
			if (_elm_lang$core$Native_Utils.cmp(n, 1) < 0) {
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$List$reverse(list),
					_1: seed
				};
			} else {
				var _p8 = generate(seed);
				var value = _p8._0;
				var newSeed = _p8._1;
				var _v2 = {ctor: '::', _0: value, _1: list},
					_v3 = n - 1,
					_v4 = generate,
					_v5 = newSeed;
				list = _v2;
				n = _v3;
				generate = _v4;
				seed = _v5;
				continue listHelp;
			}
		}
	});
var _elm_lang$core$Random$minInt = -2147483648;
var _elm_lang$core$Random$maxInt = 2147483647;
var _elm_lang$core$Random$iLogBase = F2(
	function (b, i) {
		return (_elm_lang$core$Native_Utils.cmp(i, b) < 0) ? 1 : (1 + A2(_elm_lang$core$Random$iLogBase, b, (i / b) | 0));
	});
var _elm_lang$core$Random$command = _elm_lang$core$Native_Platform.leaf('Random');
var _elm_lang$core$Random$Generator = function (a) {
	return {ctor: 'Generator', _0: a};
};
var _elm_lang$core$Random$list = F2(
	function (n, _p9) {
		var _p10 = _p9;
		return _elm_lang$core$Random$Generator(
			function (seed) {
				return A4(
					_elm_lang$core$Random$listHelp,
					{ctor: '[]'},
					n,
					_p10._0,
					seed);
			});
	});
var _elm_lang$core$Random$map = F2(
	function (func, _p11) {
		var _p12 = _p11;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p13 = _p12._0(seed0);
				var a = _p13._0;
				var seed1 = _p13._1;
				return {
					ctor: '_Tuple2',
					_0: func(a),
					_1: seed1
				};
			});
	});
var _elm_lang$core$Random$map2 = F3(
	function (func, _p15, _p14) {
		var _p16 = _p15;
		var _p17 = _p14;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p18 = _p16._0(seed0);
				var a = _p18._0;
				var seed1 = _p18._1;
				var _p19 = _p17._0(seed1);
				var b = _p19._0;
				var seed2 = _p19._1;
				return {
					ctor: '_Tuple2',
					_0: A2(func, a, b),
					_1: seed2
				};
			});
	});
var _elm_lang$core$Random$pair = F2(
	function (genA, genB) {
		return A3(
			_elm_lang$core$Random$map2,
			F2(
				function (v0, v1) {
					return {ctor: '_Tuple2', _0: v0, _1: v1};
				}),
			genA,
			genB);
	});
var _elm_lang$core$Random$map3 = F4(
	function (func, _p22, _p21, _p20) {
		var _p23 = _p22;
		var _p24 = _p21;
		var _p25 = _p20;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p26 = _p23._0(seed0);
				var a = _p26._0;
				var seed1 = _p26._1;
				var _p27 = _p24._0(seed1);
				var b = _p27._0;
				var seed2 = _p27._1;
				var _p28 = _p25._0(seed2);
				var c = _p28._0;
				var seed3 = _p28._1;
				return {
					ctor: '_Tuple2',
					_0: A3(func, a, b, c),
					_1: seed3
				};
			});
	});
var _elm_lang$core$Random$map4 = F5(
	function (func, _p32, _p31, _p30, _p29) {
		var _p33 = _p32;
		var _p34 = _p31;
		var _p35 = _p30;
		var _p36 = _p29;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p37 = _p33._0(seed0);
				var a = _p37._0;
				var seed1 = _p37._1;
				var _p38 = _p34._0(seed1);
				var b = _p38._0;
				var seed2 = _p38._1;
				var _p39 = _p35._0(seed2);
				var c = _p39._0;
				var seed3 = _p39._1;
				var _p40 = _p36._0(seed3);
				var d = _p40._0;
				var seed4 = _p40._1;
				return {
					ctor: '_Tuple2',
					_0: A4(func, a, b, c, d),
					_1: seed4
				};
			});
	});
var _elm_lang$core$Random$map5 = F6(
	function (func, _p45, _p44, _p43, _p42, _p41) {
		var _p46 = _p45;
		var _p47 = _p44;
		var _p48 = _p43;
		var _p49 = _p42;
		var _p50 = _p41;
		return _elm_lang$core$Random$Generator(
			function (seed0) {
				var _p51 = _p46._0(seed0);
				var a = _p51._0;
				var seed1 = _p51._1;
				var _p52 = _p47._0(seed1);
				var b = _p52._0;
				var seed2 = _p52._1;
				var _p53 = _p48._0(seed2);
				var c = _p53._0;
				var seed3 = _p53._1;
				var _p54 = _p49._0(seed3);
				var d = _p54._0;
				var seed4 = _p54._1;
				var _p55 = _p50._0(seed4);
				var e = _p55._0;
				var seed5 = _p55._1;
				return {
					ctor: '_Tuple2',
					_0: A5(func, a, b, c, d, e),
					_1: seed5
				};
			});
	});
var _elm_lang$core$Random$andThen = F2(
	function (callback, _p56) {
		var _p57 = _p56;
		return _elm_lang$core$Random$Generator(
			function (seed) {
				var _p58 = _p57._0(seed);
				var result = _p58._0;
				var newSeed = _p58._1;
				var _p59 = callback(result);
				var genB = _p59._0;
				return genB(newSeed);
			});
	});
var _elm_lang$core$Random$State = F2(
	function (a, b) {
		return {ctor: 'State', _0: a, _1: b};
	});
var _elm_lang$core$Random$initState = function (seed) {
	var s = A2(_elm_lang$core$Basics$max, seed, 0 - seed);
	var q = (s / (_elm_lang$core$Random$magicNum6 - 1)) | 0;
	var s2 = A2(_elm_lang$core$Basics_ops['%'], q, _elm_lang$core$Random$magicNum7 - 1);
	var s1 = A2(_elm_lang$core$Basics_ops['%'], s, _elm_lang$core$Random$magicNum6 - 1);
	return A2(_elm_lang$core$Random$State, s1 + 1, s2 + 1);
};
var _elm_lang$core$Random$next = function (_p60) {
	var _p61 = _p60;
	var _p63 = _p61._1;
	var _p62 = _p61._0;
	var k2 = (_p63 / _elm_lang$core$Random$magicNum3) | 0;
	var rawState2 = (_elm_lang$core$Random$magicNum4 * (_p63 - (k2 * _elm_lang$core$Random$magicNum3))) - (k2 * _elm_lang$core$Random$magicNum5);
	var newState2 = (_elm_lang$core$Native_Utils.cmp(rawState2, 0) < 0) ? (rawState2 + _elm_lang$core$Random$magicNum7) : rawState2;
	var k1 = (_p62 / _elm_lang$core$Random$magicNum1) | 0;
	var rawState1 = (_elm_lang$core$Random$magicNum0 * (_p62 - (k1 * _elm_lang$core$Random$magicNum1))) - (k1 * _elm_lang$core$Random$magicNum2);
	var newState1 = (_elm_lang$core$Native_Utils.cmp(rawState1, 0) < 0) ? (rawState1 + _elm_lang$core$Random$magicNum6) : rawState1;
	var z = newState1 - newState2;
	var newZ = (_elm_lang$core$Native_Utils.cmp(z, 1) < 0) ? (z + _elm_lang$core$Random$magicNum8) : z;
	return {
		ctor: '_Tuple2',
		_0: newZ,
		_1: A2(_elm_lang$core$Random$State, newState1, newState2)
	};
};
var _elm_lang$core$Random$split = function (_p64) {
	var _p65 = _p64;
	var _p68 = _p65._1;
	var _p67 = _p65._0;
	var _p66 = _elm_lang$core$Tuple$second(
		_elm_lang$core$Random$next(_p65));
	var t1 = _p66._0;
	var t2 = _p66._1;
	var new_s2 = _elm_lang$core$Native_Utils.eq(_p68, 1) ? (_elm_lang$core$Random$magicNum7 - 1) : (_p68 - 1);
	var new_s1 = _elm_lang$core$Native_Utils.eq(_p67, _elm_lang$core$Random$magicNum6 - 1) ? 1 : (_p67 + 1);
	return {
		ctor: '_Tuple2',
		_0: A2(_elm_lang$core$Random$State, new_s1, t2),
		_1: A2(_elm_lang$core$Random$State, t1, new_s2)
	};
};
var _elm_lang$core$Random$Seed = function (a) {
	return {ctor: 'Seed', _0: a};
};
var _elm_lang$core$Random$int = F2(
	function (a, b) {
		return _elm_lang$core$Random$Generator(
			function (_p69) {
				var _p70 = _p69;
				var _p75 = _p70._0;
				var base = 2147483561;
				var f = F3(
					function (n, acc, state) {
						f:
						while (true) {
							var _p71 = n;
							if (_p71 === 0) {
								return {ctor: '_Tuple2', _0: acc, _1: state};
							} else {
								var _p72 = _p75.next(state);
								var x = _p72._0;
								var nextState = _p72._1;
								var _v27 = n - 1,
									_v28 = x + (acc * base),
									_v29 = nextState;
								n = _v27;
								acc = _v28;
								state = _v29;
								continue f;
							}
						}
					});
				var _p73 = (_elm_lang$core$Native_Utils.cmp(a, b) < 0) ? {ctor: '_Tuple2', _0: a, _1: b} : {ctor: '_Tuple2', _0: b, _1: a};
				var lo = _p73._0;
				var hi = _p73._1;
				var k = (hi - lo) + 1;
				var n = A2(_elm_lang$core$Random$iLogBase, base, k);
				var _p74 = A3(f, n, 1, _p75.state);
				var v = _p74._0;
				var nextState = _p74._1;
				return {
					ctor: '_Tuple2',
					_0: lo + A2(_elm_lang$core$Basics_ops['%'], v, k),
					_1: _elm_lang$core$Random$Seed(
						_elm_lang$core$Native_Utils.update(
							_p75,
							{state: nextState}))
				};
			});
	});
var _elm_lang$core$Random$bool = A2(
	_elm_lang$core$Random$map,
	F2(
		function (x, y) {
			return _elm_lang$core$Native_Utils.eq(x, y);
		})(1),
	A2(_elm_lang$core$Random$int, 0, 1));
var _elm_lang$core$Random$float = F2(
	function (a, b) {
		return _elm_lang$core$Random$Generator(
			function (seed) {
				var _p76 = A2(
					_elm_lang$core$Random$step,
					A2(_elm_lang$core$Random$int, _elm_lang$core$Random$minInt, _elm_lang$core$Random$maxInt),
					seed);
				var number = _p76._0;
				var newSeed = _p76._1;
				var negativeOneToOne = _elm_lang$core$Basics$toFloat(number) / _elm_lang$core$Basics$toFloat(_elm_lang$core$Random$maxInt - _elm_lang$core$Random$minInt);
				var _p77 = (_elm_lang$core$Native_Utils.cmp(a, b) < 0) ? {ctor: '_Tuple2', _0: a, _1: b} : {ctor: '_Tuple2', _0: b, _1: a};
				var lo = _p77._0;
				var hi = _p77._1;
				var scaled = ((lo + hi) / 2) + ((hi - lo) * negativeOneToOne);
				return {ctor: '_Tuple2', _0: scaled, _1: newSeed};
			});
	});
var _elm_lang$core$Random$initialSeed = function (n) {
	return _elm_lang$core$Random$Seed(
		{
			state: _elm_lang$core$Random$initState(n),
			next: _elm_lang$core$Random$next,
			split: _elm_lang$core$Random$split,
			range: _elm_lang$core$Random$range
		});
};
var _elm_lang$core$Random$init = A2(
	_elm_lang$core$Task$andThen,
	function (t) {
		return _elm_lang$core$Task$succeed(
			_elm_lang$core$Random$initialSeed(
				_elm_lang$core$Basics$round(t)));
	},
	_elm_lang$core$Time$now);
var _elm_lang$core$Random$Generate = function (a) {
	return {ctor: 'Generate', _0: a};
};
var _elm_lang$core$Random$generate = F2(
	function (tagger, generator) {
		return _elm_lang$core$Random$command(
			_elm_lang$core$Random$Generate(
				A2(_elm_lang$core$Random$map, tagger, generator)));
	});
var _elm_lang$core$Random$cmdMap = F2(
	function (func, _p78) {
		var _p79 = _p78;
		return _elm_lang$core$Random$Generate(
			A2(_elm_lang$core$Random$map, func, _p79._0));
	});
_elm_lang$core$Native_Platform.effectManagers['Random'] = {pkg: 'elm-lang/core', init: _elm_lang$core$Random$init, onEffects: _elm_lang$core$Random$onEffects, onSelfMsg: _elm_lang$core$Random$onSelfMsg, tag: 'cmd', cmdMap: _elm_lang$core$Random$cmdMap};

var _elm_lang$virtual_dom$VirtualDom_Debug$wrap;
var _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags;

var _elm_lang$virtual_dom$Native_VirtualDom = function() {

var STYLE_KEY = 'STYLE';
var EVENT_KEY = 'EVENT';
var ATTR_KEY = 'ATTR';
var ATTR_NS_KEY = 'ATTR_NS';

var localDoc = typeof document !== 'undefined' ? document : {};


////////////  VIRTUAL DOM NODES  ////////////


function text(string)
{
	return {
		type: 'text',
		text: string
	};
}


function node(tag)
{
	return F2(function(factList, kidList) {
		return nodeHelp(tag, factList, kidList);
	});
}


function nodeHelp(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function keyedNode(tag, factList, kidList)
{
	var organized = organizeFacts(factList);
	var namespace = organized.namespace;
	var facts = organized.facts;

	var children = [];
	var descendantsCount = 0;
	while (kidList.ctor !== '[]')
	{
		var kid = kidList._0;
		descendantsCount += (kid._1.descendantsCount || 0);
		children.push(kid);
		kidList = kidList._1;
	}
	descendantsCount += children.length;

	return {
		type: 'keyed-node',
		tag: tag,
		facts: facts,
		children: children,
		namespace: namespace,
		descendantsCount: descendantsCount
	};
}


function custom(factList, model, impl)
{
	var facts = organizeFacts(factList).facts;

	return {
		type: 'custom',
		facts: facts,
		model: model,
		impl: impl
	};
}


function map(tagger, node)
{
	return {
		type: 'tagger',
		tagger: tagger,
		node: node,
		descendantsCount: 1 + (node.descendantsCount || 0)
	};
}


function thunk(func, args, thunk)
{
	return {
		type: 'thunk',
		func: func,
		args: args,
		thunk: thunk,
		node: undefined
	};
}

function lazy(fn, a)
{
	return thunk(fn, [a], function() {
		return fn(a);
	});
}

function lazy2(fn, a, b)
{
	return thunk(fn, [a,b], function() {
		return A2(fn, a, b);
	});
}

function lazy3(fn, a, b, c)
{
	return thunk(fn, [a,b,c], function() {
		return A3(fn, a, b, c);
	});
}



// FACTS


function organizeFacts(factList)
{
	var namespace, facts = {};

	while (factList.ctor !== '[]')
	{
		var entry = factList._0;
		var key = entry.key;

		if (key === ATTR_KEY || key === ATTR_NS_KEY || key === EVENT_KEY)
		{
			var subFacts = facts[key] || {};
			subFacts[entry.realKey] = entry.value;
			facts[key] = subFacts;
		}
		else if (key === STYLE_KEY)
		{
			var styles = facts[key] || {};
			var styleList = entry.value;
			while (styleList.ctor !== '[]')
			{
				var style = styleList._0;
				styles[style._0] = style._1;
				styleList = styleList._1;
			}
			facts[key] = styles;
		}
		else if (key === 'namespace')
		{
			namespace = entry.value;
		}
		else if (key === 'className')
		{
			var classes = facts[key];
			facts[key] = typeof classes === 'undefined'
				? entry.value
				: classes + ' ' + entry.value;
		}
 		else
		{
			facts[key] = entry.value;
		}
		factList = factList._1;
	}

	return {
		facts: facts,
		namespace: namespace
	};
}



////////////  PROPERTIES AND ATTRIBUTES  ////////////


function style(value)
{
	return {
		key: STYLE_KEY,
		value: value
	};
}


function property(key, value)
{
	return {
		key: key,
		value: value
	};
}


function attribute(key, value)
{
	return {
		key: ATTR_KEY,
		realKey: key,
		value: value
	};
}


function attributeNS(namespace, key, value)
{
	return {
		key: ATTR_NS_KEY,
		realKey: key,
		value: {
			value: value,
			namespace: namespace
		}
	};
}


function on(name, options, decoder)
{
	return {
		key: EVENT_KEY,
		realKey: name,
		value: {
			options: options,
			decoder: decoder
		}
	};
}


function equalEvents(a, b)
{
	if (a.options !== b.options)
	{
		if (a.options.stopPropagation !== b.options.stopPropagation || a.options.preventDefault !== b.options.preventDefault)
		{
			return false;
		}
	}
	return _elm_lang$core$Native_Json.equality(a.decoder, b.decoder);
}


function mapProperty(func, property)
{
	if (property.key !== EVENT_KEY)
	{
		return property;
	}
	return on(
		property.realKey,
		property.value.options,
		A2(_elm_lang$core$Json_Decode$map, func, property.value.decoder)
	);
}


////////////  RENDER  ////////////


function render(vNode, eventNode)
{
	switch (vNode.type)
	{
		case 'thunk':
			if (!vNode.node)
			{
				vNode.node = vNode.thunk();
			}
			return render(vNode.node, eventNode);

		case 'tagger':
			var subNode = vNode.node;
			var tagger = vNode.tagger;

			while (subNode.type === 'tagger')
			{
				typeof tagger !== 'object'
					? tagger = [tagger, subNode.tagger]
					: tagger.push(subNode.tagger);

				subNode = subNode.node;
			}

			var subEventRoot = { tagger: tagger, parent: eventNode };
			var domNode = render(subNode, subEventRoot);
			domNode.elm_event_node_ref = subEventRoot;
			return domNode;

		case 'text':
			return localDoc.createTextNode(vNode.text);

		case 'node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i], eventNode));
			}

			return domNode;

		case 'keyed-node':
			var domNode = vNode.namespace
				? localDoc.createElementNS(vNode.namespace, vNode.tag)
				: localDoc.createElement(vNode.tag);

			applyFacts(domNode, eventNode, vNode.facts);

			var children = vNode.children;

			for (var i = 0; i < children.length; i++)
			{
				domNode.appendChild(render(children[i]._1, eventNode));
			}

			return domNode;

		case 'custom':
			var domNode = vNode.impl.render(vNode.model);
			applyFacts(domNode, eventNode, vNode.facts);
			return domNode;
	}
}



////////////  APPLY FACTS  ////////////


function applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		switch (key)
		{
			case STYLE_KEY:
				applyStyles(domNode, value);
				break;

			case EVENT_KEY:
				applyEvents(domNode, eventNode, value);
				break;

			case ATTR_KEY:
				applyAttrs(domNode, value);
				break;

			case ATTR_NS_KEY:
				applyAttrsNS(domNode, value);
				break;

			case 'value':
				if (domNode[key] !== value)
				{
					domNode[key] = value;
				}
				break;

			default:
				domNode[key] = value;
				break;
		}
	}
}

function applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}

function applyEvents(domNode, eventNode, events)
{
	var allHandlers = domNode.elm_handlers || {};

	for (var key in events)
	{
		var handler = allHandlers[key];
		var value = events[key];

		if (typeof value === 'undefined')
		{
			domNode.removeEventListener(key, handler);
			allHandlers[key] = undefined;
		}
		else if (typeof handler === 'undefined')
		{
			var handler = makeEventHandler(eventNode, value);
			domNode.addEventListener(key, handler);
			allHandlers[key] = handler;
		}
		else
		{
			handler.info = value;
		}
	}

	domNode.elm_handlers = allHandlers;
}

function makeEventHandler(eventNode, info)
{
	function eventHandler(event)
	{
		var info = eventHandler.info;

		var value = A2(_elm_lang$core$Native_Json.run, info.decoder, event);

		if (value.ctor === 'Ok')
		{
			var options = info.options;
			if (options.stopPropagation)
			{
				event.stopPropagation();
			}
			if (options.preventDefault)
			{
				event.preventDefault();
			}

			var message = value._0;

			var currentEventNode = eventNode;
			while (currentEventNode)
			{
				var tagger = currentEventNode.tagger;
				if (typeof tagger === 'function')
				{
					message = tagger(message);
				}
				else
				{
					for (var i = tagger.length; i--; )
					{
						message = tagger[i](message);
					}
				}
				currentEventNode = currentEventNode.parent;
			}
		}
	};

	eventHandler.info = info;

	return eventHandler;
}

function applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		if (typeof value === 'undefined')
		{
			domNode.removeAttribute(key);
		}
		else
		{
			domNode.setAttribute(key, value);
		}
	}
}

function applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.namespace;
		var value = pair.value;

		if (typeof value === 'undefined')
		{
			domNode.removeAttributeNS(namespace, key);
		}
		else
		{
			domNode.setAttributeNS(namespace, key, value);
		}
	}
}



////////////  DIFF  ////////////


function diff(a, b)
{
	var patches = [];
	diffHelp(a, b, patches, 0);
	return patches;
}


function makePatch(type, index, data)
{
	return {
		index: index,
		type: type,
		data: data,
		domNode: undefined,
		eventNode: undefined
	};
}


function diffHelp(a, b, patches, index)
{
	if (a === b)
	{
		return;
	}

	var aType = a.type;
	var bType = b.type;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (aType !== bType)
	{
		patches.push(makePatch('p-redraw', index, b));
		return;
	}

	// Now we know that both nodes are the same type.
	switch (bType)
	{
		case 'thunk':
			var aArgs = a.args;
			var bArgs = b.args;
			var i = aArgs.length;
			var same = a.func === b.func && i === bArgs.length;
			while (same && i--)
			{
				same = aArgs[i] === bArgs[i];
			}
			if (same)
			{
				b.node = a.node;
				return;
			}
			b.node = b.thunk();
			var subPatches = [];
			diffHelp(a.node, b.node, subPatches, 0);
			if (subPatches.length > 0)
			{
				patches.push(makePatch('p-thunk', index, subPatches));
			}
			return;

		case 'tagger':
			// gather nested taggers
			var aTaggers = a.tagger;
			var bTaggers = b.tagger;
			var nesting = false;

			var aSubNode = a.node;
			while (aSubNode.type === 'tagger')
			{
				nesting = true;

				typeof aTaggers !== 'object'
					? aTaggers = [aTaggers, aSubNode.tagger]
					: aTaggers.push(aSubNode.tagger);

				aSubNode = aSubNode.node;
			}

			var bSubNode = b.node;
			while (bSubNode.type === 'tagger')
			{
				nesting = true;

				typeof bTaggers !== 'object'
					? bTaggers = [bTaggers, bSubNode.tagger]
					: bTaggers.push(bSubNode.tagger);

				bSubNode = bSubNode.node;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && aTaggers.length !== bTaggers.length)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !pairwiseRefEqual(aTaggers, bTaggers) : aTaggers !== bTaggers)
			{
				patches.push(makePatch('p-tagger', index, bTaggers));
			}

			// diff everything below the taggers
			diffHelp(aSubNode, bSubNode, patches, index + 1);
			return;

		case 'text':
			if (a.text !== b.text)
			{
				patches.push(makePatch('p-text', index, b.text));
				return;
			}

			return;

		case 'node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffChildren(a, b, patches, index);
			return;

		case 'keyed-node':
			// Bail if obvious indicators have changed. Implies more serious
			// structural changes such that it's not worth it to diff.
			if (a.tag !== b.tag || a.namespace !== b.namespace)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);

			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			diffKeyedChildren(a, b, patches, index);
			return;

		case 'custom':
			if (a.impl !== b.impl)
			{
				patches.push(makePatch('p-redraw', index, b));
				return;
			}

			var factsDiff = diffFacts(a.facts, b.facts);
			if (typeof factsDiff !== 'undefined')
			{
				patches.push(makePatch('p-facts', index, factsDiff));
			}

			var patch = b.impl.diff(a,b);
			if (patch)
			{
				patches.push(makePatch('p-custom', index, patch));
				return;
			}

			return;
	}
}


// assumes the incoming arrays are the same length
function pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function diffFacts(a, b, category)
{
	var diff;

	// look for changes and removals
	for (var aKey in a)
	{
		if (aKey === STYLE_KEY || aKey === EVENT_KEY || aKey === ATTR_KEY || aKey === ATTR_NS_KEY)
		{
			var subDiff = diffFacts(a[aKey], b[aKey] || {}, aKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[aKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(aKey in b))
		{
			diff = diff || {};
			diff[aKey] =
				(typeof category === 'undefined')
					? (typeof a[aKey] === 'string' ? '' : null)
					:
				(category === STYLE_KEY)
					? ''
					:
				(category === EVENT_KEY || category === ATTR_KEY)
					? undefined
					:
				{ namespace: a[aKey].namespace, value: undefined };

			continue;
		}

		var aValue = a[aKey];
		var bValue = b[aKey];

		// reference equal, so don't worry about it
		if (aValue === bValue && aKey !== 'value'
			|| category === EVENT_KEY && equalEvents(aValue, bValue))
		{
			continue;
		}

		diff = diff || {};
		diff[aKey] = bValue;
	}

	// add new stuff
	for (var bKey in b)
	{
		if (!(bKey in a))
		{
			diff = diff || {};
			diff[bKey] = b[bKey];
		}
	}

	return diff;
}


function diffChildren(aParent, bParent, patches, rootIndex)
{
	var aChildren = aParent.children;
	var bChildren = bParent.children;

	var aLen = aChildren.length;
	var bLen = bChildren.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (aLen > bLen)
	{
		patches.push(makePatch('p-remove-last', rootIndex, aLen - bLen));
	}
	else if (aLen < bLen)
	{
		patches.push(makePatch('p-append', rootIndex, bChildren.slice(aLen)));
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	var index = rootIndex;
	var minLen = aLen < bLen ? aLen : bLen;
	for (var i = 0; i < minLen; i++)
	{
		index++;
		var aChild = aChildren[i];
		diffHelp(aChild, bChildren[i], patches, index);
		index += aChild.descendantsCount || 0;
	}
}



////////////  KEYED DIFF  ////////////


function diffKeyedChildren(aParent, bParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var aChildren = aParent.children;
	var bChildren = bParent.children;
	var aLen = aChildren.length;
	var bLen = bChildren.length;
	var aIndex = 0;
	var bIndex = 0;

	var index = rootIndex;

	while (aIndex < aLen && bIndex < bLen)
	{
		var a = aChildren[aIndex];
		var b = bChildren[bIndex];

		var aKey = a._0;
		var bKey = b._0;
		var aNode = a._1;
		var bNode = b._1;

		// check if keys match

		if (aKey === bKey)
		{
			index++;
			diffHelp(aNode, bNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex++;
			bIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var aLookAhead = aIndex + 1 < aLen;
		var bLookAhead = bIndex + 1 < bLen;

		if (aLookAhead)
		{
			var aNext = aChildren[aIndex + 1];
			var aNextKey = aNext._0;
			var aNextNode = aNext._1;
			var oldMatch = bKey === aNextKey;
		}

		if (bLookAhead)
		{
			var bNext = bChildren[bIndex + 1];
			var bNextKey = bNext._0;
			var bNextNode = bNext._1;
			var newMatch = aKey === bNextKey;
		}


		// swap a and b
		if (aLookAhead && bLookAhead && newMatch && oldMatch)
		{
			index++;
			diffHelp(aNode, bNextNode, localPatches, index);
			insertNode(changes, localPatches, aKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			removeNode(changes, localPatches, aKey, aNextNode, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		// insert b
		if (bLookAhead && newMatch)
		{
			index++;
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			diffHelp(aNode, bNextNode, localPatches, index);
			index += aNode.descendantsCount || 0;

			aIndex += 1;
			bIndex += 2;
			continue;
		}

		// remove a
		if (aLookAhead && oldMatch)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 1;
			continue;
		}

		// remove a, insert b
		if (aLookAhead && bLookAhead && aNextKey === bNextKey)
		{
			index++;
			removeNode(changes, localPatches, aKey, aNode, index);
			insertNode(changes, localPatches, bKey, bNode, bIndex, inserts);
			index += aNode.descendantsCount || 0;

			index++;
			diffHelp(aNextNode, bNextNode, localPatches, index);
			index += aNextNode.descendantsCount || 0;

			aIndex += 2;
			bIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (aIndex < aLen)
	{
		index++;
		var a = aChildren[aIndex];
		var aNode = a._1;
		removeNode(changes, localPatches, a._0, aNode, index);
		index += aNode.descendantsCount || 0;
		aIndex++;
	}

	var endInserts;
	while (bIndex < bLen)
	{
		endInserts = endInserts || [];
		var b = bChildren[bIndex];
		insertNode(changes, localPatches, b._0, b._1, undefined, endInserts);
		bIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || typeof endInserts !== 'undefined')
	{
		patches.push(makePatch('p-reorder', rootIndex, {
			patches: localPatches,
			inserts: inserts,
			endInserts: endInserts
		}));
	}
}



////////////  CHANGES FROM KEYED DIFF  ////////////


var POSTFIX = '_elmW6BL';


function insertNode(changes, localPatches, key, vnode, bIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		entry = {
			tag: 'insert',
			vnode: vnode,
			index: bIndex,
			data: undefined
		};

		inserts.push({ index: bIndex, entry: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.tag === 'remove')
	{
		inserts.push({ index: bIndex, entry: entry });

		entry.tag = 'move';
		var subPatches = [];
		diffHelp(entry.vnode, vnode, subPatches, entry.index);
		entry.index = bIndex;
		entry.data.data = {
			patches: subPatches,
			entry: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	insertNode(changes, localPatches, key + POSTFIX, vnode, bIndex, inserts);
}


function removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (typeof entry === 'undefined')
	{
		var patch = makePatch('p-remove', index, undefined);
		localPatches.push(patch);

		changes[key] = {
			tag: 'remove',
			vnode: vnode,
			index: index,
			data: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.tag === 'insert')
	{
		entry.tag = 'move';
		var subPatches = [];
		diffHelp(vnode, entry.vnode, subPatches, index);

		var patch = makePatch('p-remove', index, {
			patches: subPatches,
			entry: entry
		});
		localPatches.push(patch);

		return;
	}

	// this key has already been removed or moved, a duplicate!
	removeNode(changes, localPatches, key + POSTFIX, vnode, index);
}



////////////  ADD DOM NODES  ////////////
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function addDomNodes(domNode, vNode, patches, eventNode)
{
	addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.descendantsCount, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.index;

	while (index === low)
	{
		var patchType = patch.type;

		if (patchType === 'p-thunk')
		{
			addDomNodes(domNode, vNode.node, patch.data, eventNode);
		}
		else if (patchType === 'p-reorder')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var subPatches = patch.data.patches;
			if (subPatches.length > 0)
			{
				addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 'p-remove')
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;

			var data = patch.data;
			if (typeof data !== 'undefined')
			{
				data.entry.data = domNode;
				var subPatches = data.patches;
				if (subPatches.length > 0)
				{
					addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.domNode = domNode;
			patch.eventNode = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.index) > high)
		{
			return i;
		}
	}

	switch (vNode.type)
	{
		case 'tagger':
			var subNode = vNode.node;

			while (subNode.type === "tagger")
			{
				subNode = subNode.node;
			}

			return addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);

		case 'node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j];
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'keyed-node':
			var vChildren = vNode.children;
			var childNodes = domNode.childNodes;
			for (var j = 0; j < vChildren.length; j++)
			{
				low++;
				var vChild = vChildren[j]._1;
				var nextLow = low + (vChild.descendantsCount || 0);
				if (low <= index && index <= nextLow)
				{
					i = addDomNodesHelp(childNodes[j], vChild, patches, i, low, nextLow, eventNode);
					if (!(patch = patches[i]) || (index = patch.index) > high)
					{
						return i;
					}
				}
				low = nextLow;
			}
			return i;

		case 'text':
		case 'thunk':
			throw new Error('should never traverse `text` or `thunk` nodes like this');
	}
}



////////////  APPLY PATCHES  ////////////


function applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return applyPatchesHelp(rootDomNode, patches);
}

function applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.domNode
		var newNode = applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function applyPatch(domNode, patch)
{
	switch (patch.type)
	{
		case 'p-redraw':
			return applyPatchRedraw(domNode, patch.data, patch.eventNode);

		case 'p-facts':
			applyFacts(domNode, patch.eventNode, patch.data);
			return domNode;

		case 'p-text':
			domNode.replaceData(0, domNode.length, patch.data);
			return domNode;

		case 'p-thunk':
			return applyPatchesHelp(domNode, patch.data);

		case 'p-tagger':
			if (typeof domNode.elm_event_node_ref !== 'undefined')
			{
				domNode.elm_event_node_ref.tagger = patch.data;
			}
			else
			{
				domNode.elm_event_node_ref = { tagger: patch.data, parent: patch.eventNode };
			}
			return domNode;

		case 'p-remove-last':
			var i = patch.data;
			while (i--)
			{
				domNode.removeChild(domNode.lastChild);
			}
			return domNode;

		case 'p-append':
			var newNodes = patch.data;
			for (var i = 0; i < newNodes.length; i++)
			{
				domNode.appendChild(render(newNodes[i], patch.eventNode));
			}
			return domNode;

		case 'p-remove':
			var data = patch.data;
			if (typeof data === 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.entry;
			if (typeof entry.index !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.data = applyPatchesHelp(domNode, data.patches);
			return domNode;

		case 'p-reorder':
			return applyPatchReorder(domNode, patch);

		case 'p-custom':
			var impl = patch.data;
			return impl.applyPatch(domNode, impl.data);

		default:
			throw new Error('Ran into an unknown patch!');
	}
}


function applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = render(vNode, eventNode);

	if (typeof newNode.elm_event_node_ref === 'undefined')
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function applyPatchReorder(domNode, patch)
{
	var data = patch.data;

	// remove end inserts
	var frag = applyPatchReorderEndInsertsHelp(data.endInserts, patch);

	// removals
	domNode = applyPatchesHelp(domNode, data.patches);

	// inserts
	var inserts = data.inserts;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.entry;
		var node = entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode);
		domNode.insertBefore(node, domNode.childNodes[insert.index]);
	}

	// add end inserts
	if (typeof frag !== 'undefined')
	{
		domNode.appendChild(frag);
	}

	return domNode;
}


function applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (typeof endInserts === 'undefined')
	{
		return;
	}

	var frag = localDoc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.entry;
		frag.appendChild(entry.tag === 'move'
			? entry.data
			: render(entry.vnode, patch.eventNode)
		);
	}
	return frag;
}


// PROGRAMS

var program = makeProgram(checkNoFlags);
var programWithFlags = makeProgram(checkYesFlags);

function makeProgram(flagChecker)
{
	return F2(function(debugWrap, impl)
	{
		return function(flagDecoder)
		{
			return function(object, moduleName, debugMetadata)
			{
				var checker = flagChecker(flagDecoder, moduleName);
				if (typeof debugMetadata === 'undefined')
				{
					normalSetup(impl, object, moduleName, checker);
				}
				else
				{
					debugSetup(A2(debugWrap, debugMetadata, impl), object, moduleName, checker);
				}
			};
		};
	});
}

function staticProgram(vNode)
{
	var nothing = _elm_lang$core$Native_Utils.Tuple2(
		_elm_lang$core$Native_Utils.Tuple0,
		_elm_lang$core$Platform_Cmd$none
	);
	return A2(program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, {
		init: nothing,
		view: function() { return vNode; },
		update: F2(function() { return nothing; }),
		subscriptions: function() { return _elm_lang$core$Platform_Sub$none; }
	})();
}


// FLAG CHECKERS

function checkNoFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flags === 'undefined')
		{
			return init;
		}

		var errorMessage =
			'The `' + moduleName + '` module does not need flags.\n'
			+ 'Initialize it with no arguments and you should be all set!';

		crash(errorMessage, domNode);
	};
}

function checkYesFlags(flagDecoder, moduleName)
{
	return function(init, flags, domNode)
	{
		if (typeof flagDecoder === 'undefined')
		{
			var errorMessage =
				'Are you trying to sneak a Never value into Elm? Trickster!\n'
				+ 'It looks like ' + moduleName + '.main is defined with `programWithFlags` but has type `Program Never`.\n'
				+ 'Use `program` instead if you do not want flags.'

			crash(errorMessage, domNode);
		}

		var result = A2(_elm_lang$core$Native_Json.run, flagDecoder, flags);
		if (result.ctor === 'Ok')
		{
			return init(result._0);
		}

		var errorMessage =
			'Trying to initialize the `' + moduleName + '` module with an unexpected flag.\n'
			+ 'I tried to convert it to an Elm value, but ran into this problem:\n\n'
			+ result._0;

		crash(errorMessage, domNode);
	};
}

function crash(errorMessage, domNode)
{
	if (domNode)
	{
		domNode.innerHTML =
			'<div style="padding-left:1em;">'
			+ '<h2 style="font-weight:normal;"><b>Oops!</b> Something went wrong when starting your Elm program.</h2>'
			+ '<pre style="padding-left:1em;">' + errorMessage + '</pre>'
			+ '</div>';
	}

	throw new Error(errorMessage);
}


//  NORMAL SETUP

function normalSetup(impl, object, moduleName, flagChecker)
{
	object['embed'] = function embed(node, flags)
	{
		while (node.lastChild)
		{
			node.removeChild(node.lastChild);
		}

		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update,
			impl.subscriptions,
			normalRenderer(node, impl.view)
		);
	};

	object['fullscreen'] = function fullscreen(flags)
	{
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update,
			impl.subscriptions,
			normalRenderer(document.body, impl.view)
		);
	};
}

function normalRenderer(parentNode, view)
{
	return function(tagger, initialModel)
	{
		var eventNode = { tagger: tagger, parent: undefined };
		var initialVirtualNode = view(initialModel);
		var domNode = render(initialVirtualNode, eventNode);
		parentNode.appendChild(domNode);
		return makeStepper(domNode, view, initialVirtualNode, eventNode);
	};
}


// STEPPER

var rAF =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };

function makeStepper(domNode, view, initialVirtualNode, eventNode)
{
	var state = 'NO_REQUEST';
	var currNode = initialVirtualNode;
	var nextModel;

	function updateIfNeeded()
	{
		switch (state)
		{
			case 'NO_REQUEST':
				throw new Error(
					'Unexpected draw callback.\n' +
					'Please report this to <https://github.com/elm-lang/virtual-dom/issues>.'
				);

			case 'PENDING_REQUEST':
				rAF(updateIfNeeded);
				state = 'EXTRA_REQUEST';

				var nextNode = view(nextModel);
				var patches = diff(currNode, nextNode);
				domNode = applyPatches(domNode, currNode, patches, eventNode);
				currNode = nextNode;

				return;

			case 'EXTRA_REQUEST':
				state = 'NO_REQUEST';
				return;
		}
	}

	return function stepper(model)
	{
		if (state === 'NO_REQUEST')
		{
			rAF(updateIfNeeded);
		}
		state = 'PENDING_REQUEST';
		nextModel = model;
	};
}


// DEBUG SETUP

function debugSetup(impl, object, moduleName, flagChecker)
{
	object['fullscreen'] = function fullscreen(flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, document.body),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, document.body, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};

	object['embed'] = function fullscreen(node, flags)
	{
		var popoutRef = { doc: undefined };
		return _elm_lang$core$Native_Platform.initialize(
			flagChecker(impl.init, flags, node),
			impl.update(scrollTask(popoutRef)),
			impl.subscriptions,
			debugRenderer(moduleName, node, popoutRef, impl.view, impl.viewIn, impl.viewOut)
		);
	};
}

function scrollTask(popoutRef)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var doc = popoutRef.doc;
		if (doc)
		{
			var msgs = doc.getElementsByClassName('debugger-sidebar-messages')[0];
			if (msgs)
			{
				msgs.scrollTop = msgs.scrollHeight;
			}
		}
		callback(_elm_lang$core$Native_Scheduler.succeed(_elm_lang$core$Native_Utils.Tuple0));
	});
}


function debugRenderer(moduleName, parentNode, popoutRef, view, viewIn, viewOut)
{
	return function(tagger, initialModel)
	{
		var appEventNode = { tagger: tagger, parent: undefined };
		var eventNode = { tagger: tagger, parent: undefined };

		// make normal stepper
		var appVirtualNode = view(initialModel);
		var appNode = render(appVirtualNode, appEventNode);
		parentNode.appendChild(appNode);
		var appStepper = makeStepper(appNode, view, appVirtualNode, appEventNode);

		// make overlay stepper
		var overVirtualNode = viewIn(initialModel)._1;
		var overNode = render(overVirtualNode, eventNode);
		parentNode.appendChild(overNode);
		var wrappedViewIn = wrapViewIn(appEventNode, overNode, viewIn);
		var overStepper = makeStepper(overNode, wrappedViewIn, overVirtualNode, eventNode);

		// make debugger stepper
		var debugStepper = makeDebugStepper(initialModel, viewOut, eventNode, parentNode, moduleName, popoutRef);

		return function stepper(model)
		{
			appStepper(model);
			overStepper(model);
			debugStepper(model);
		}
	};
}

function makeDebugStepper(initialModel, view, eventNode, parentNode, moduleName, popoutRef)
{
	var curr;
	var domNode;

	return function stepper(model)
	{
		if (!model.isDebuggerOpen)
		{
			return;
		}

		if (!popoutRef.doc)
		{
			curr = view(model);
			domNode = openDebugWindow(moduleName, popoutRef, curr, eventNode);
			return;
		}

		// switch to document of popout
		localDoc = popoutRef.doc;

		var next = view(model);
		var patches = diff(curr, next);
		domNode = applyPatches(domNode, curr, patches, eventNode);
		curr = next;

		// switch back to normal document
		localDoc = document;
	};
}

function openDebugWindow(moduleName, popoutRef, virtualNode, eventNode)
{
	var w = 900;
	var h = 360;
	var x = screen.width - w;
	var y = screen.height - h;
	var debugWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);

	// switch to window document
	localDoc = debugWindow.document;

	popoutRef.doc = localDoc;
	localDoc.title = 'Debugger - ' + moduleName;
	localDoc.body.style.margin = '0';
	localDoc.body.style.padding = '0';
	var domNode = render(virtualNode, eventNode);
	localDoc.body.appendChild(domNode);

	localDoc.addEventListener('keydown', function(event) {
		if (event.metaKey && event.which === 82)
		{
			window.location.reload();
		}
		if (event.which === 38)
		{
			eventNode.tagger({ ctor: 'Up' });
			event.preventDefault();
		}
		if (event.which === 40)
		{
			eventNode.tagger({ ctor: 'Down' });
			event.preventDefault();
		}
	});

	function close()
	{
		popoutRef.doc = undefined;
		debugWindow.close();
	}
	window.addEventListener('unload', close);
	debugWindow.addEventListener('unload', function() {
		popoutRef.doc = undefined;
		window.removeEventListener('unload', close);
		eventNode.tagger({ ctor: 'Close' });
	});

	// switch back to the normal document
	localDoc = document;

	return domNode;
}


// BLOCK EVENTS

function wrapViewIn(appEventNode, overlayNode, viewIn)
{
	var ignorer = makeIgnorer(overlayNode);
	var blocking = 'Normal';
	var overflow;

	var normalTagger = appEventNode.tagger;
	var blockTagger = function() {};

	return function(model)
	{
		var tuple = viewIn(model);
		var newBlocking = tuple._0.ctor;
		appEventNode.tagger = newBlocking === 'Normal' ? normalTagger : blockTagger;
		if (blocking !== newBlocking)
		{
			traverse('removeEventListener', ignorer, blocking);
			traverse('addEventListener', ignorer, newBlocking);

			if (blocking === 'Normal')
			{
				overflow = document.body.style.overflow;
				document.body.style.overflow = 'hidden';
			}

			if (newBlocking === 'Normal')
			{
				document.body.style.overflow = overflow;
			}

			blocking = newBlocking;
		}
		return tuple._1;
	}
}

function traverse(verbEventListener, ignorer, blocking)
{
	switch(blocking)
	{
		case 'Normal':
			return;

		case 'Pause':
			return traverseHelp(verbEventListener, ignorer, mostEvents);

		case 'Message':
			return traverseHelp(verbEventListener, ignorer, allEvents);
	}
}

function traverseHelp(verbEventListener, handler, eventNames)
{
	for (var i = 0; i < eventNames.length; i++)
	{
		document.body[verbEventListener](eventNames[i], handler, true);
	}
}

function makeIgnorer(overlayNode)
{
	return function(event)
	{
		if (event.type === 'keydown' && event.metaKey && event.which === 82)
		{
			return;
		}

		var isScroll = event.type === 'scroll' || event.type === 'wheel';

		var node = event.target;
		while (node !== null)
		{
			if (node.className === 'elm-overlay-message-details' && isScroll)
			{
				return;
			}

			if (node === overlayNode && !isScroll)
			{
				return;
			}
			node = node.parentNode;
		}

		event.stopPropagation();
		event.preventDefault();
	}
}

var mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var allEvents = mostEvents.concat('wheel', 'scroll');


return {
	node: node,
	text: text,
	custom: custom,
	map: F2(map),

	on: F3(on),
	style: style,
	property: F2(property),
	attribute: F2(attribute),
	attributeNS: F3(attributeNS),
	mapProperty: F2(mapProperty),

	lazy: F2(lazy),
	lazy2: F3(lazy2),
	lazy3: F4(lazy3),
	keyedNode: F3(keyedNode),

	program: program,
	programWithFlags: programWithFlags,
	staticProgram: staticProgram
};

}();

var _elm_lang$virtual_dom$VirtualDom$programWithFlags = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.programWithFlags, _elm_lang$virtual_dom$VirtualDom_Debug$wrapWithFlags, impl);
};
var _elm_lang$virtual_dom$VirtualDom$program = function (impl) {
	return A2(_elm_lang$virtual_dom$Native_VirtualDom.program, _elm_lang$virtual_dom$VirtualDom_Debug$wrap, impl);
};
var _elm_lang$virtual_dom$VirtualDom$keyedNode = _elm_lang$virtual_dom$Native_VirtualDom.keyedNode;
var _elm_lang$virtual_dom$VirtualDom$lazy3 = _elm_lang$virtual_dom$Native_VirtualDom.lazy3;
var _elm_lang$virtual_dom$VirtualDom$lazy2 = _elm_lang$virtual_dom$Native_VirtualDom.lazy2;
var _elm_lang$virtual_dom$VirtualDom$lazy = _elm_lang$virtual_dom$Native_VirtualDom.lazy;
var _elm_lang$virtual_dom$VirtualDom$defaultOptions = {stopPropagation: false, preventDefault: false};
var _elm_lang$virtual_dom$VirtualDom$onWithOptions = _elm_lang$virtual_dom$Native_VirtualDom.on;
var _elm_lang$virtual_dom$VirtualDom$on = F2(
	function (eventName, decoder) {
		return A3(_elm_lang$virtual_dom$VirtualDom$onWithOptions, eventName, _elm_lang$virtual_dom$VirtualDom$defaultOptions, decoder);
	});
var _elm_lang$virtual_dom$VirtualDom$style = _elm_lang$virtual_dom$Native_VirtualDom.style;
var _elm_lang$virtual_dom$VirtualDom$mapProperty = _elm_lang$virtual_dom$Native_VirtualDom.mapProperty;
var _elm_lang$virtual_dom$VirtualDom$attributeNS = _elm_lang$virtual_dom$Native_VirtualDom.attributeNS;
var _elm_lang$virtual_dom$VirtualDom$attribute = _elm_lang$virtual_dom$Native_VirtualDom.attribute;
var _elm_lang$virtual_dom$VirtualDom$property = _elm_lang$virtual_dom$Native_VirtualDom.property;
var _elm_lang$virtual_dom$VirtualDom$map = _elm_lang$virtual_dom$Native_VirtualDom.map;
var _elm_lang$virtual_dom$VirtualDom$text = _elm_lang$virtual_dom$Native_VirtualDom.text;
var _elm_lang$virtual_dom$VirtualDom$node = _elm_lang$virtual_dom$Native_VirtualDom.node;
var _elm_lang$virtual_dom$VirtualDom$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});
var _elm_lang$virtual_dom$VirtualDom$Node = {ctor: 'Node'};
var _elm_lang$virtual_dom$VirtualDom$Property = {ctor: 'Property'};

var _elm_lang$html$Html$programWithFlags = _elm_lang$virtual_dom$VirtualDom$programWithFlags;
var _elm_lang$html$Html$program = _elm_lang$virtual_dom$VirtualDom$program;
var _elm_lang$html$Html$beginnerProgram = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html$program(
		{
			init: A2(
				_elm_lang$core$Platform_Cmd_ops['!'],
				_p1.model,
				{ctor: '[]'}),
			update: F2(
				function (msg, model) {
					return A2(
						_elm_lang$core$Platform_Cmd_ops['!'],
						A2(_p1.update, msg, model),
						{ctor: '[]'});
				}),
			view: _p1.view,
			subscriptions: function (_p2) {
				return _elm_lang$core$Platform_Sub$none;
			}
		});
};
var _elm_lang$html$Html$map = _elm_lang$virtual_dom$VirtualDom$map;
var _elm_lang$html$Html$text = _elm_lang$virtual_dom$VirtualDom$text;
var _elm_lang$html$Html$node = _elm_lang$virtual_dom$VirtualDom$node;
var _elm_lang$html$Html$body = _elm_lang$html$Html$node('body');
var _elm_lang$html$Html$section = _elm_lang$html$Html$node('section');
var _elm_lang$html$Html$nav = _elm_lang$html$Html$node('nav');
var _elm_lang$html$Html$article = _elm_lang$html$Html$node('article');
var _elm_lang$html$Html$aside = _elm_lang$html$Html$node('aside');
var _elm_lang$html$Html$h1 = _elm_lang$html$Html$node('h1');
var _elm_lang$html$Html$h2 = _elm_lang$html$Html$node('h2');
var _elm_lang$html$Html$h3 = _elm_lang$html$Html$node('h3');
var _elm_lang$html$Html$h4 = _elm_lang$html$Html$node('h4');
var _elm_lang$html$Html$h5 = _elm_lang$html$Html$node('h5');
var _elm_lang$html$Html$h6 = _elm_lang$html$Html$node('h6');
var _elm_lang$html$Html$header = _elm_lang$html$Html$node('header');
var _elm_lang$html$Html$footer = _elm_lang$html$Html$node('footer');
var _elm_lang$html$Html$address = _elm_lang$html$Html$node('address');
var _elm_lang$html$Html$main_ = _elm_lang$html$Html$node('main');
var _elm_lang$html$Html$p = _elm_lang$html$Html$node('p');
var _elm_lang$html$Html$hr = _elm_lang$html$Html$node('hr');
var _elm_lang$html$Html$pre = _elm_lang$html$Html$node('pre');
var _elm_lang$html$Html$blockquote = _elm_lang$html$Html$node('blockquote');
var _elm_lang$html$Html$ol = _elm_lang$html$Html$node('ol');
var _elm_lang$html$Html$ul = _elm_lang$html$Html$node('ul');
var _elm_lang$html$Html$li = _elm_lang$html$Html$node('li');
var _elm_lang$html$Html$dl = _elm_lang$html$Html$node('dl');
var _elm_lang$html$Html$dt = _elm_lang$html$Html$node('dt');
var _elm_lang$html$Html$dd = _elm_lang$html$Html$node('dd');
var _elm_lang$html$Html$figure = _elm_lang$html$Html$node('figure');
var _elm_lang$html$Html$figcaption = _elm_lang$html$Html$node('figcaption');
var _elm_lang$html$Html$div = _elm_lang$html$Html$node('div');
var _elm_lang$html$Html$a = _elm_lang$html$Html$node('a');
var _elm_lang$html$Html$em = _elm_lang$html$Html$node('em');
var _elm_lang$html$Html$strong = _elm_lang$html$Html$node('strong');
var _elm_lang$html$Html$small = _elm_lang$html$Html$node('small');
var _elm_lang$html$Html$s = _elm_lang$html$Html$node('s');
var _elm_lang$html$Html$cite = _elm_lang$html$Html$node('cite');
var _elm_lang$html$Html$q = _elm_lang$html$Html$node('q');
var _elm_lang$html$Html$dfn = _elm_lang$html$Html$node('dfn');
var _elm_lang$html$Html$abbr = _elm_lang$html$Html$node('abbr');
var _elm_lang$html$Html$time = _elm_lang$html$Html$node('time');
var _elm_lang$html$Html$code = _elm_lang$html$Html$node('code');
var _elm_lang$html$Html$var = _elm_lang$html$Html$node('var');
var _elm_lang$html$Html$samp = _elm_lang$html$Html$node('samp');
var _elm_lang$html$Html$kbd = _elm_lang$html$Html$node('kbd');
var _elm_lang$html$Html$sub = _elm_lang$html$Html$node('sub');
var _elm_lang$html$Html$sup = _elm_lang$html$Html$node('sup');
var _elm_lang$html$Html$i = _elm_lang$html$Html$node('i');
var _elm_lang$html$Html$b = _elm_lang$html$Html$node('b');
var _elm_lang$html$Html$u = _elm_lang$html$Html$node('u');
var _elm_lang$html$Html$mark = _elm_lang$html$Html$node('mark');
var _elm_lang$html$Html$ruby = _elm_lang$html$Html$node('ruby');
var _elm_lang$html$Html$rt = _elm_lang$html$Html$node('rt');
var _elm_lang$html$Html$rp = _elm_lang$html$Html$node('rp');
var _elm_lang$html$Html$bdi = _elm_lang$html$Html$node('bdi');
var _elm_lang$html$Html$bdo = _elm_lang$html$Html$node('bdo');
var _elm_lang$html$Html$span = _elm_lang$html$Html$node('span');
var _elm_lang$html$Html$br = _elm_lang$html$Html$node('br');
var _elm_lang$html$Html$wbr = _elm_lang$html$Html$node('wbr');
var _elm_lang$html$Html$ins = _elm_lang$html$Html$node('ins');
var _elm_lang$html$Html$del = _elm_lang$html$Html$node('del');
var _elm_lang$html$Html$img = _elm_lang$html$Html$node('img');
var _elm_lang$html$Html$iframe = _elm_lang$html$Html$node('iframe');
var _elm_lang$html$Html$embed = _elm_lang$html$Html$node('embed');
var _elm_lang$html$Html$object = _elm_lang$html$Html$node('object');
var _elm_lang$html$Html$param = _elm_lang$html$Html$node('param');
var _elm_lang$html$Html$video = _elm_lang$html$Html$node('video');
var _elm_lang$html$Html$audio = _elm_lang$html$Html$node('audio');
var _elm_lang$html$Html$source = _elm_lang$html$Html$node('source');
var _elm_lang$html$Html$track = _elm_lang$html$Html$node('track');
var _elm_lang$html$Html$canvas = _elm_lang$html$Html$node('canvas');
var _elm_lang$html$Html$math = _elm_lang$html$Html$node('math');
var _elm_lang$html$Html$table = _elm_lang$html$Html$node('table');
var _elm_lang$html$Html$caption = _elm_lang$html$Html$node('caption');
var _elm_lang$html$Html$colgroup = _elm_lang$html$Html$node('colgroup');
var _elm_lang$html$Html$col = _elm_lang$html$Html$node('col');
var _elm_lang$html$Html$tbody = _elm_lang$html$Html$node('tbody');
var _elm_lang$html$Html$thead = _elm_lang$html$Html$node('thead');
var _elm_lang$html$Html$tfoot = _elm_lang$html$Html$node('tfoot');
var _elm_lang$html$Html$tr = _elm_lang$html$Html$node('tr');
var _elm_lang$html$Html$td = _elm_lang$html$Html$node('td');
var _elm_lang$html$Html$th = _elm_lang$html$Html$node('th');
var _elm_lang$html$Html$form = _elm_lang$html$Html$node('form');
var _elm_lang$html$Html$fieldset = _elm_lang$html$Html$node('fieldset');
var _elm_lang$html$Html$legend = _elm_lang$html$Html$node('legend');
var _elm_lang$html$Html$label = _elm_lang$html$Html$node('label');
var _elm_lang$html$Html$input = _elm_lang$html$Html$node('input');
var _elm_lang$html$Html$button = _elm_lang$html$Html$node('button');
var _elm_lang$html$Html$select = _elm_lang$html$Html$node('select');
var _elm_lang$html$Html$datalist = _elm_lang$html$Html$node('datalist');
var _elm_lang$html$Html$optgroup = _elm_lang$html$Html$node('optgroup');
var _elm_lang$html$Html$option = _elm_lang$html$Html$node('option');
var _elm_lang$html$Html$textarea = _elm_lang$html$Html$node('textarea');
var _elm_lang$html$Html$keygen = _elm_lang$html$Html$node('keygen');
var _elm_lang$html$Html$output = _elm_lang$html$Html$node('output');
var _elm_lang$html$Html$progress = _elm_lang$html$Html$node('progress');
var _elm_lang$html$Html$meter = _elm_lang$html$Html$node('meter');
var _elm_lang$html$Html$details = _elm_lang$html$Html$node('details');
var _elm_lang$html$Html$summary = _elm_lang$html$Html$node('summary');
var _elm_lang$html$Html$menuitem = _elm_lang$html$Html$node('menuitem');
var _elm_lang$html$Html$menu = _elm_lang$html$Html$node('menu');

var _elm_lang$html$Html_Attributes$map = _elm_lang$virtual_dom$VirtualDom$mapProperty;
var _elm_lang$html$Html_Attributes$attribute = _elm_lang$virtual_dom$VirtualDom$attribute;
var _elm_lang$html$Html_Attributes$contextmenu = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'contextmenu', value);
};
var _elm_lang$html$Html_Attributes$draggable = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'draggable', value);
};
var _elm_lang$html$Html_Attributes$itemprop = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'itemprop', value);
};
var _elm_lang$html$Html_Attributes$tabindex = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'tabIndex',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$charset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'charset', value);
};
var _elm_lang$html$Html_Attributes$height = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'height',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$width = function (value) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'width',
		_elm_lang$core$Basics$toString(value));
};
var _elm_lang$html$Html_Attributes$formaction = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'formAction', value);
};
var _elm_lang$html$Html_Attributes$list = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'list', value);
};
var _elm_lang$html$Html_Attributes$minlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'minLength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$maxlength = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'maxlength',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$size = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'size',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$form = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'form', value);
};
var _elm_lang$html$Html_Attributes$cols = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'cols',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rows = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rows',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$challenge = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'challenge', value);
};
var _elm_lang$html$Html_Attributes$media = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'media', value);
};
var _elm_lang$html$Html_Attributes$rel = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'rel', value);
};
var _elm_lang$html$Html_Attributes$datetime = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'datetime', value);
};
var _elm_lang$html$Html_Attributes$pubdate = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'pubdate', value);
};
var _elm_lang$html$Html_Attributes$colspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'colspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$rowspan = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$attribute,
		'rowspan',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$manifest = function (value) {
	return A2(_elm_lang$html$Html_Attributes$attribute, 'manifest', value);
};
var _elm_lang$html$Html_Attributes$property = _elm_lang$virtual_dom$VirtualDom$property;
var _elm_lang$html$Html_Attributes$stringProperty = F2(
	function (name, string) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$string(string));
	});
var _elm_lang$html$Html_Attributes$class = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'className', name);
};
var _elm_lang$html$Html_Attributes$id = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'id', name);
};
var _elm_lang$html$Html_Attributes$title = function (name) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'title', name);
};
var _elm_lang$html$Html_Attributes$accesskey = function ($char) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'accessKey',
		_elm_lang$core$String$fromChar($char));
};
var _elm_lang$html$Html_Attributes$dir = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dir', value);
};
var _elm_lang$html$Html_Attributes$dropzone = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'dropzone', value);
};
var _elm_lang$html$Html_Attributes$lang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'lang', value);
};
var _elm_lang$html$Html_Attributes$content = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'content', value);
};
var _elm_lang$html$Html_Attributes$httpEquiv = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'httpEquiv', value);
};
var _elm_lang$html$Html_Attributes$language = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'language', value);
};
var _elm_lang$html$Html_Attributes$src = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'src', value);
};
var _elm_lang$html$Html_Attributes$alt = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'alt', value);
};
var _elm_lang$html$Html_Attributes$preload = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'preload', value);
};
var _elm_lang$html$Html_Attributes$poster = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'poster', value);
};
var _elm_lang$html$Html_Attributes$kind = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'kind', value);
};
var _elm_lang$html$Html_Attributes$srclang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srclang', value);
};
var _elm_lang$html$Html_Attributes$sandbox = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'sandbox', value);
};
var _elm_lang$html$Html_Attributes$srcdoc = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'srcdoc', value);
};
var _elm_lang$html$Html_Attributes$type_ = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'type', value);
};
var _elm_lang$html$Html_Attributes$value = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'value', value);
};
var _elm_lang$html$Html_Attributes$defaultValue = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'defaultValue', value);
};
var _elm_lang$html$Html_Attributes$placeholder = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'placeholder', value);
};
var _elm_lang$html$Html_Attributes$accept = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'accept', value);
};
var _elm_lang$html$Html_Attributes$acceptCharset = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'acceptCharset', value);
};
var _elm_lang$html$Html_Attributes$action = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'action', value);
};
var _elm_lang$html$Html_Attributes$autocomplete = function (bool) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var _elm_lang$html$Html_Attributes$enctype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'enctype', value);
};
var _elm_lang$html$Html_Attributes$method = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'method', value);
};
var _elm_lang$html$Html_Attributes$name = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'name', value);
};
var _elm_lang$html$Html_Attributes$pattern = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'pattern', value);
};
var _elm_lang$html$Html_Attributes$for = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'htmlFor', value);
};
var _elm_lang$html$Html_Attributes$max = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'max', value);
};
var _elm_lang$html$Html_Attributes$min = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'min', value);
};
var _elm_lang$html$Html_Attributes$step = function (n) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'step', n);
};
var _elm_lang$html$Html_Attributes$wrap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'wrap', value);
};
var _elm_lang$html$Html_Attributes$usemap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'useMap', value);
};
var _elm_lang$html$Html_Attributes$shape = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'shape', value);
};
var _elm_lang$html$Html_Attributes$coords = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'coords', value);
};
var _elm_lang$html$Html_Attributes$keytype = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'keytype', value);
};
var _elm_lang$html$Html_Attributes$align = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'align', value);
};
var _elm_lang$html$Html_Attributes$cite = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'cite', value);
};
var _elm_lang$html$Html_Attributes$href = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'href', value);
};
var _elm_lang$html$Html_Attributes$target = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'target', value);
};
var _elm_lang$html$Html_Attributes$downloadAs = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'download', value);
};
var _elm_lang$html$Html_Attributes$hreflang = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'hreflang', value);
};
var _elm_lang$html$Html_Attributes$ping = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'ping', value);
};
var _elm_lang$html$Html_Attributes$start = function (n) {
	return A2(
		_elm_lang$html$Html_Attributes$stringProperty,
		'start',
		_elm_lang$core$Basics$toString(n));
};
var _elm_lang$html$Html_Attributes$headers = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'headers', value);
};
var _elm_lang$html$Html_Attributes$scope = function (value) {
	return A2(_elm_lang$html$Html_Attributes$stringProperty, 'scope', value);
};
var _elm_lang$html$Html_Attributes$boolProperty = F2(
	function (name, bool) {
		return A2(
			_elm_lang$html$Html_Attributes$property,
			name,
			_elm_lang$core$Json_Encode$bool(bool));
	});
var _elm_lang$html$Html_Attributes$hidden = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'hidden', bool);
};
var _elm_lang$html$Html_Attributes$contenteditable = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'contentEditable', bool);
};
var _elm_lang$html$Html_Attributes$spellcheck = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'spellcheck', bool);
};
var _elm_lang$html$Html_Attributes$async = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'async', bool);
};
var _elm_lang$html$Html_Attributes$defer = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'defer', bool);
};
var _elm_lang$html$Html_Attributes$scoped = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'scoped', bool);
};
var _elm_lang$html$Html_Attributes$autoplay = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autoplay', bool);
};
var _elm_lang$html$Html_Attributes$controls = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'controls', bool);
};
var _elm_lang$html$Html_Attributes$loop = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'loop', bool);
};
var _elm_lang$html$Html_Attributes$default = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'default', bool);
};
var _elm_lang$html$Html_Attributes$seamless = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'seamless', bool);
};
var _elm_lang$html$Html_Attributes$checked = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'checked', bool);
};
var _elm_lang$html$Html_Attributes$selected = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'selected', bool);
};
var _elm_lang$html$Html_Attributes$autofocus = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'autofocus', bool);
};
var _elm_lang$html$Html_Attributes$disabled = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'disabled', bool);
};
var _elm_lang$html$Html_Attributes$multiple = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'multiple', bool);
};
var _elm_lang$html$Html_Attributes$novalidate = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'noValidate', bool);
};
var _elm_lang$html$Html_Attributes$readonly = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'readOnly', bool);
};
var _elm_lang$html$Html_Attributes$required = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'required', bool);
};
var _elm_lang$html$Html_Attributes$ismap = function (value) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'isMap', value);
};
var _elm_lang$html$Html_Attributes$download = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'download', bool);
};
var _elm_lang$html$Html_Attributes$reversed = function (bool) {
	return A2(_elm_lang$html$Html_Attributes$boolProperty, 'reversed', bool);
};
var _elm_lang$html$Html_Attributes$classList = function (list) {
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			A2(
				_elm_lang$core$List$map,
				_elm_lang$core$Tuple$first,
				A2(_elm_lang$core$List$filter, _elm_lang$core$Tuple$second, list))));
};
var _elm_lang$html$Html_Attributes$style = _elm_lang$virtual_dom$VirtualDom$style;

var _elm_lang$html$Html_Events$keyCode = A2(_elm_lang$core$Json_Decode$field, 'keyCode', _elm_lang$core$Json_Decode$int);
var _elm_lang$html$Html_Events$targetChecked = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'checked',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$bool);
var _elm_lang$html$Html_Events$targetValue = A2(
	_elm_lang$core$Json_Decode$at,
	{
		ctor: '::',
		_0: 'target',
		_1: {
			ctor: '::',
			_0: 'value',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$string);
var _elm_lang$html$Html_Events$defaultOptions = _elm_lang$virtual_dom$VirtualDom$defaultOptions;
var _elm_lang$html$Html_Events$onWithOptions = _elm_lang$virtual_dom$VirtualDom$onWithOptions;
var _elm_lang$html$Html_Events$on = _elm_lang$virtual_dom$VirtualDom$on;
var _elm_lang$html$Html_Events$onFocus = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'focus',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onBlur = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'blur',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onSubmitOptions = _elm_lang$core$Native_Utils.update(
	_elm_lang$html$Html_Events$defaultOptions,
	{preventDefault: true});
var _elm_lang$html$Html_Events$onSubmit = function (msg) {
	return A3(
		_elm_lang$html$Html_Events$onWithOptions,
		'submit',
		_elm_lang$html$Html_Events$onSubmitOptions,
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onCheck = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'change',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetChecked));
};
var _elm_lang$html$Html_Events$onInput = function (tagger) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'input',
		A2(_elm_lang$core$Json_Decode$map, tagger, _elm_lang$html$Html_Events$targetValue));
};
var _elm_lang$html$Html_Events$onMouseOut = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseout',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseOver = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseover',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseLeave = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseleave',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseEnter = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseenter',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseUp = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mouseup',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onMouseDown = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'mousedown',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onDoubleClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'dblclick',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$onClick = function (msg) {
	return A2(
		_elm_lang$html$Html_Events$on,
		'click',
		_elm_lang$core$Json_Decode$succeed(msg));
};
var _elm_lang$html$Html_Events$Options = F2(
	function (a, b) {
		return {stopPropagation: a, preventDefault: b};
	});

var _elm_lang$html$Html_Keyed$node = _elm_lang$virtual_dom$VirtualDom$keyedNode;
var _elm_lang$html$Html_Keyed$ol = _elm_lang$html$Html_Keyed$node('ol');
var _elm_lang$html$Html_Keyed$ul = _elm_lang$html$Html_Keyed$node('ul');

var _elm_lang$http$Native_Http = function() {


// ENCODING AND DECODING

function encodeUri(string)
{
	return encodeURIComponent(string);
}

function decodeUri(string)
{
	try
	{
		return _elm_lang$core$Maybe$Just(decodeURIComponent(string));
	}
	catch(e)
	{
		return _elm_lang$core$Maybe$Nothing;
	}
}


// SEND REQUEST

function toTask(request, maybeProgress)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		var xhr = new XMLHttpRequest();

		configureProgress(xhr, maybeProgress);

		xhr.addEventListener('error', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NetworkError' }));
		});
		xhr.addEventListener('timeout', function() {
			callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'Timeout' }));
		});
		xhr.addEventListener('load', function() {
			callback(handleResponse(xhr, request.expect.responseToResult));
		});

		try
		{
			xhr.open(request.method, request.url, true);
		}
		catch (e)
		{
			return callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'BadUrl', _0: request.url }));
		}

		configureRequest(xhr, request);
		send(xhr, request.body);

		return function() { xhr.abort(); };
	});
}

function configureProgress(xhr, maybeProgress)
{
	if (maybeProgress.ctor === 'Nothing')
	{
		return;
	}

	xhr.addEventListener('progress', function(event) {
		if (!event.lengthComputable)
		{
			return;
		}
		_elm_lang$core$Native_Scheduler.rawSpawn(maybeProgress._0({
			bytes: event.loaded,
			bytesExpected: event.total
		}));
	});
}

function configureRequest(xhr, request)
{
	function setHeader(pair)
	{
		xhr.setRequestHeader(pair._0, pair._1);
	}

	A2(_elm_lang$core$List$map, setHeader, request.headers);
	xhr.responseType = request.expect.responseType;
	xhr.withCredentials = request.withCredentials;

	if (request.timeout.ctor === 'Just')
	{
		xhr.timeout = request.timeout._0;
	}
}

function send(xhr, body)
{
	switch (body.ctor)
	{
		case 'EmptyBody':
			xhr.send();
			return;

		case 'StringBody':
			xhr.setRequestHeader('Content-Type', body._0);
			xhr.send(body._1);
			return;

		case 'FormDataBody':
			xhr.send(body._0);
			return;
	}
}


// RESPONSES

function handleResponse(xhr, responseToResult)
{
	var response = toResponse(xhr);

	if (xhr.status < 200 || 300 <= xhr.status)
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadStatus',
			_0: response
		});
	}

	var result = responseToResult(response);

	if (result.ctor === 'Ok')
	{
		return _elm_lang$core$Native_Scheduler.succeed(result._0);
	}
	else
	{
		response.body = xhr.responseText;
		return _elm_lang$core$Native_Scheduler.fail({
			ctor: 'BadPayload',
			_0: result._0,
			_1: response
		});
	}
}

function toResponse(xhr)
{
	return {
		status: { code: xhr.status, message: xhr.statusText },
		headers: parseHeaders(xhr.getAllResponseHeaders()),
		url: xhr.responseURL,
		body: xhr.response
	};
}

function parseHeaders(rawHeaders)
{
	var headers = _elm_lang$core$Dict$empty;

	if (!rawHeaders)
	{
		return headers;
	}

	var headerPairs = rawHeaders.split('\u000d\u000a');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf('\u003a\u0020');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(_elm_lang$core$Dict$update, key, function(oldValue) {
				if (oldValue.ctor === 'Just')
				{
					return _elm_lang$core$Maybe$Just(value + ', ' + oldValue._0);
				}
				return _elm_lang$core$Maybe$Just(value);
			}, headers);
		}
	}

	return headers;
}


// EXPECTORS

function expectStringResponse(responseToResult)
{
	return {
		responseType: 'text',
		responseToResult: responseToResult
	};
}

function mapExpect(func, expect)
{
	return {
		responseType: expect.responseType,
		responseToResult: function(response) {
			var convertedResponse = expect.responseToResult(response);
			return A2(_elm_lang$core$Result$map, func, convertedResponse);
		}
	};
}


// BODY

function multipart(parts)
{
	var formData = new FormData();

	while (parts.ctor !== '[]')
	{
		var part = parts._0;
		formData.append(part._0, part._1);
		parts = parts._1;
	}

	return { ctor: 'FormDataBody', _0: formData };
}

return {
	toTask: F2(toTask),
	expectStringResponse: expectStringResponse,
	mapExpect: F2(mapExpect),
	multipart: multipart,
	encodeUri: encodeUri,
	decodeUri: decodeUri
};

}();

var _elm_lang$http$Http_Internal$map = F2(
	function (func, request) {
		return _elm_lang$core$Native_Utils.update(
			request,
			{
				expect: A2(_elm_lang$http$Native_Http.mapExpect, func, request.expect)
			});
	});
var _elm_lang$http$Http_Internal$RawRequest = F7(
	function (a, b, c, d, e, f, g) {
		return {method: a, headers: b, url: c, body: d, expect: e, timeout: f, withCredentials: g};
	});
var _elm_lang$http$Http_Internal$Request = function (a) {
	return {ctor: 'Request', _0: a};
};
var _elm_lang$http$Http_Internal$Expect = {ctor: 'Expect'};
var _elm_lang$http$Http_Internal$FormDataBody = {ctor: 'FormDataBody'};
var _elm_lang$http$Http_Internal$StringBody = F2(
	function (a, b) {
		return {ctor: 'StringBody', _0: a, _1: b};
	});
var _elm_lang$http$Http_Internal$EmptyBody = {ctor: 'EmptyBody'};
var _elm_lang$http$Http_Internal$Header = F2(
	function (a, b) {
		return {ctor: 'Header', _0: a, _1: b};
	});

var _elm_lang$http$Http$decodeUri = _elm_lang$http$Native_Http.decodeUri;
var _elm_lang$http$Http$encodeUri = _elm_lang$http$Native_Http.encodeUri;
var _elm_lang$http$Http$expectStringResponse = _elm_lang$http$Native_Http.expectStringResponse;
var _elm_lang$http$Http$expectJson = function (decoder) {
	return _elm_lang$http$Http$expectStringResponse(
		function (response) {
			return A2(_elm_lang$core$Json_Decode$decodeString, decoder, response.body);
		});
};
var _elm_lang$http$Http$expectString = _elm_lang$http$Http$expectStringResponse(
	function (response) {
		return _elm_lang$core$Result$Ok(response.body);
	});
var _elm_lang$http$Http$multipartBody = _elm_lang$http$Native_Http.multipart;
var _elm_lang$http$Http$stringBody = _elm_lang$http$Http_Internal$StringBody;
var _elm_lang$http$Http$jsonBody = function (value) {
	return A2(
		_elm_lang$http$Http_Internal$StringBody,
		'application/json',
		A2(_elm_lang$core$Json_Encode$encode, 0, value));
};
var _elm_lang$http$Http$emptyBody = _elm_lang$http$Http_Internal$EmptyBody;
var _elm_lang$http$Http$header = _elm_lang$http$Http_Internal$Header;
var _elm_lang$http$Http$request = _elm_lang$http$Http_Internal$Request;
var _elm_lang$http$Http$post = F3(
	function (url, body, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'POST',
				headers: {ctor: '[]'},
				url: url,
				body: body,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$get = F2(
	function (url, decoder) {
		return _elm_lang$http$Http$request(
			{
				method: 'GET',
				headers: {ctor: '[]'},
				url: url,
				body: _elm_lang$http$Http$emptyBody,
				expect: _elm_lang$http$Http$expectJson(decoder),
				timeout: _elm_lang$core$Maybe$Nothing,
				withCredentials: false
			});
	});
var _elm_lang$http$Http$getString = function (url) {
	return _elm_lang$http$Http$request(
		{
			method: 'GET',
			headers: {ctor: '[]'},
			url: url,
			body: _elm_lang$http$Http$emptyBody,
			expect: _elm_lang$http$Http$expectString,
			timeout: _elm_lang$core$Maybe$Nothing,
			withCredentials: false
		});
};
var _elm_lang$http$Http$toTask = function (_p0) {
	var _p1 = _p0;
	return A2(_elm_lang$http$Native_Http.toTask, _p1._0, _elm_lang$core$Maybe$Nothing);
};
var _elm_lang$http$Http$send = F2(
	function (resultToMessage, request) {
		return A2(
			_elm_lang$core$Task$attempt,
			resultToMessage,
			_elm_lang$http$Http$toTask(request));
	});
var _elm_lang$http$Http$Response = F4(
	function (a, b, c, d) {
		return {url: a, status: b, headers: c, body: d};
	});
var _elm_lang$http$Http$BadPayload = F2(
	function (a, b) {
		return {ctor: 'BadPayload', _0: a, _1: b};
	});
var _elm_lang$http$Http$BadStatus = function (a) {
	return {ctor: 'BadStatus', _0: a};
};
var _elm_lang$http$Http$NetworkError = {ctor: 'NetworkError'};
var _elm_lang$http$Http$Timeout = {ctor: 'Timeout'};
var _elm_lang$http$Http$BadUrl = function (a) {
	return {ctor: 'BadUrl', _0: a};
};
var _elm_lang$http$Http$StringPart = F2(
	function (a, b) {
		return {ctor: 'StringPart', _0: a, _1: b};
	});
var _elm_lang$http$Http$stringPart = _elm_lang$http$Http$StringPart;

var _pablen$toasty$Toasty$getNewId = function (seed) {
	return A2(
		_elm_lang$core$Random$step,
		A2(_elm_lang$core$Random$int, _elm_lang$core$Random$minInt, _elm_lang$core$Random$maxInt),
		seed);
};
var _pablen$toasty$Toasty$Stack = F2(
	function (a, b) {
		return {ctor: 'Stack', _0: a, _1: b};
	});
var _pablen$toasty$Toasty$initialState = A2(
	_pablen$toasty$Toasty$Stack,
	{ctor: '[]'},
	_elm_lang$core$Random$initialSeed(0));
var _pablen$toasty$Toasty$TransitionOut = function (a) {
	return {ctor: 'TransitionOut', _0: a};
};
var _pablen$toasty$Toasty$itemContainer = F4(
	function (_p1, tagger, _p0, toastView) {
		var _p2 = _p1;
		var _p6 = _p2._0;
		var _p3 = _p0;
		var _p5 = _p3._0;
		var attrs = function () {
			var _p4 = _p3._1;
			if (_p4.ctor === 'Entered') {
				return _p6.transitionInAttrs;
			} else {
				return _p6.transitionOutAttrs;
			}
		}();
		return {
			ctor: '_Tuple2',
			_0: _elm_lang$core$Basics$toString(_p5),
			_1: A2(
				_elm_lang$html$Html$li,
				A2(
					_elm_lang$core$Basics_ops['++'],
					_p6.itemAttrs,
					A2(
						_elm_lang$core$Basics_ops['++'],
						attrs,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onClick(
								tagger(
									_pablen$toasty$Toasty$TransitionOut(_p5))),
							_1: {ctor: '[]'}
						})),
				{
					ctor: '::',
					_0: toastView(_p3._2),
					_1: {ctor: '[]'}
				})
		};
	});
var _pablen$toasty$Toasty$view = F4(
	function (config, toastView, tagger, _p7) {
		var _p8 = _p7;
		var _p10 = _p8._0;
		var _p9 = config;
		var cfg = _p9._0;
		return _elm_lang$core$List$isEmpty(_p10) ? _elm_lang$html$Html$text('') : A2(
			_elm_lang$html$Html_Keyed$ol,
			cfg.containerAttrs,
			A2(
				_elm_lang$core$List$map,
				function (toast) {
					return A4(_pablen$toasty$Toasty$itemContainer, config, tagger, toast, toastView);
				},
				_p10));
	});
var _pablen$toasty$Toasty$Remove = function (a) {
	return {ctor: 'Remove', _0: a};
};
var _pablen$toasty$Toasty$Add = function (a) {
	return {ctor: 'Add', _0: a};
};
var _pablen$toasty$Toasty$Config = function (a) {
	return {ctor: 'Config', _0: a};
};
var _pablen$toasty$Toasty$config = _pablen$toasty$Toasty$Config(
	{
		transitionOutDuration: 0,
		transitionOutAttrs: {ctor: '[]'},
		transitionInAttrs: {ctor: '[]'},
		containerAttrs: {ctor: '[]'},
		itemAttrs: {ctor: '[]'},
		delay: 5000
	});
var _pablen$toasty$Toasty$transitionOutDuration = F2(
	function (time, _p11) {
		var _p12 = _p11;
		return _pablen$toasty$Toasty$Config(
			_elm_lang$core$Native_Utils.update(
				_p12._0,
				{transitionOutDuration: time}));
	});
var _pablen$toasty$Toasty$transitionInAttrs = F2(
	function (attrs, _p13) {
		var _p14 = _p13;
		return _pablen$toasty$Toasty$Config(
			_elm_lang$core$Native_Utils.update(
				_p14._0,
				{transitionInAttrs: attrs}));
	});
var _pablen$toasty$Toasty$transitionOutAttrs = F2(
	function (attrs, _p15) {
		var _p16 = _p15;
		return _pablen$toasty$Toasty$Config(
			_elm_lang$core$Native_Utils.update(
				_p16._0,
				{transitionOutAttrs: attrs}));
	});
var _pablen$toasty$Toasty$containerAttrs = F2(
	function (attrs, _p17) {
		var _p18 = _p17;
		return _pablen$toasty$Toasty$Config(
			_elm_lang$core$Native_Utils.update(
				_p18._0,
				{containerAttrs: attrs}));
	});
var _pablen$toasty$Toasty$itemAttrs = F2(
	function (attrs, _p19) {
		var _p20 = _p19;
		return _pablen$toasty$Toasty$Config(
			_elm_lang$core$Native_Utils.update(
				_p20._0,
				{itemAttrs: attrs}));
	});
var _pablen$toasty$Toasty$delay = F2(
	function (time, _p21) {
		var _p22 = _p21;
		return _pablen$toasty$Toasty$Config(
			_elm_lang$core$Native_Utils.update(
				_p22._0,
				{delay: time}));
	});
var _pablen$toasty$Toasty$Leaving = {ctor: 'Leaving'};
var _pablen$toasty$Toasty$Entered = {ctor: 'Entered'};
var _pablen$toasty$Toasty$addToast = F4(
	function (config, tagger, toast, _p23) {
		var _p24 = _p23;
		var _p30 = _p24._0;
		var _p25 = _p30.toasties;
		var toasts = _p25._0;
		var seed = _p25._1;
		var _p26 = _pablen$toasty$Toasty$getNewId(seed);
		var newId = _p26._0;
		var newSeed = _p26._1;
		var _p27 = config;
		var cfg = _p27._0;
		return A2(
			_elm_lang$core$Platform_Cmd_ops['!'],
			_elm_lang$core$Native_Utils.update(
				_p30,
				{
					toasties: A2(
						_pablen$toasty$Toasty$Stack,
						A2(
							_elm_lang$core$Basics_ops['++'],
							toasts,
							{
								ctor: '::',
								_0: {ctor: '_Tuple3', _0: newId, _1: _pablen$toasty$Toasty$Entered, _2: toast},
								_1: {ctor: '[]'}
							}),
						newSeed)
				}),
			{
				ctor: '::',
				_0: _p24._1,
				_1: {
					ctor: '::',
					_0: A2(
						_elm_lang$core$Task$perform,
						function (_p28) {
							var _p29 = _p28;
							return tagger(
								_pablen$toasty$Toasty$TransitionOut(newId));
						},
						_elm_lang$core$Process$sleep(cfg.delay * _elm_lang$core$Time$millisecond)),
					_1: {ctor: '[]'}
				}
			});
	});
var _pablen$toasty$Toasty$update = F4(
	function (config, tagger, msg, model) {
		var _p31 = model.toasties;
		var toasts = _p31._0;
		var seed = _p31._1;
		var _p32 = config;
		var cfg = _p32._0;
		var _p33 = msg;
		switch (_p33.ctor) {
			case 'Add':
				return A4(
					_pablen$toasty$Toasty$addToast,
					config,
					tagger,
					_p33._0,
					{ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none});
			case 'Remove':
				var newStack = A2(
					_elm_lang$core$List$filter,
					function (_p34) {
						var _p35 = _p34;
						return !_elm_lang$core$Native_Utils.eq(_p35._0, _p33._0);
					},
					toasts);
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{
							toasties: A2(_pablen$toasty$Toasty$Stack, newStack, seed)
						}),
					{ctor: '[]'});
			default:
				var _p41 = _p33._0;
				var newStack = A2(
					_elm_lang$core$List$map,
					function (_p36) {
						var _p37 = _p36;
						var _p39 = _p37._2;
						var _p38 = _p37._0;
						return _elm_lang$core$Native_Utils.eq(_p38, _p41) ? {ctor: '_Tuple3', _0: _p38, _1: _pablen$toasty$Toasty$Leaving, _2: _p39} : {ctor: '_Tuple3', _0: _p38, _1: _p37._1, _2: _p39};
					},
					toasts);
				return A2(
					_elm_lang$core$Platform_Cmd_ops['!'],
					_elm_lang$core$Native_Utils.update(
						model,
						{
							toasties: A2(_pablen$toasty$Toasty$Stack, newStack, seed)
						}),
					{
						ctor: '::',
						_0: A2(
							_elm_lang$core$Task$perform,
							function (_p40) {
								return tagger(
									_pablen$toasty$Toasty$Remove(_p41));
							},
							_elm_lang$core$Process$sleep(cfg.transitionOutDuration * _elm_lang$core$Time$millisecond)),
						_1: {ctor: '[]'}
					});
		}
	});

var _pablen$toasty$Toasty_Defaults$genericToast = F3(
	function (variantClass, title, message) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('toasty-container'),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class(variantClass),
					_1: {ctor: '[]'}
				}
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$h1,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('toasty-title'),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(title),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: _elm_lang$core$String$isEmpty(message) ? _elm_lang$html$Html$text('') : A2(
						_elm_lang$html$Html$p,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$class('toasty-message'),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _elm_lang$html$Html$text(message),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}
			});
	});
var _pablen$toasty$Toasty_Defaults$view = function (toast) {
	var _p0 = toast;
	switch (_p0.ctor) {
		case 'Success':
			return A3(_pablen$toasty$Toasty_Defaults$genericToast, 'toasty-success', _p0._0, _p0._1);
		case 'Warning':
			return A3(_pablen$toasty$Toasty_Defaults$genericToast, 'toasty-warning', _p0._0, _p0._1);
		default:
			return A3(_pablen$toasty$Toasty_Defaults$genericToast, 'toasty-error', _p0._0, _p0._1);
	}
};
var _pablen$toasty$Toasty_Defaults$transitionOutAttrs = {
	ctor: '::',
	_0: _elm_lang$html$Html_Attributes$class('animated fadeOutRightBig'),
	_1: {
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$style(
			{
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'max-height', _1: '0'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'margin-top', _1: '0'},
					_1: {ctor: '[]'}
				}
			}),
		_1: {ctor: '[]'}
	}
};
var _pablen$toasty$Toasty_Defaults$transitionInAttrs = {
	ctor: '::',
	_0: _elm_lang$html$Html_Attributes$class('animated bounceInRight'),
	_1: {ctor: '[]'}
};
var _pablen$toasty$Toasty_Defaults$itemAttrs = {
	ctor: '::',
	_0: _elm_lang$html$Html_Attributes$style(
		{
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'margin', _1: '1em 1em 0 1em'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'max-height', _1: '100px'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'transition', _1: 'max-height 0.6s, margin-top 0.6s'},
					_1: {ctor: '[]'}
				}
			}
		}),
	_1: {ctor: '[]'}
};
var _pablen$toasty$Toasty_Defaults$containerAttrs = {
	ctor: '::',
	_0: _elm_lang$html$Html_Attributes$style(
		{
			ctor: '::',
			_0: {ctor: '_Tuple2', _0: 'position', _1: 'fixed'},
			_1: {
				ctor: '::',
				_0: {ctor: '_Tuple2', _0: 'top', _1: '0'},
				_1: {
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'right', _1: '0'},
					_1: {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'width', _1: '100%'},
						_1: {
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'max-width', _1: '300px'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'list-style-type', _1: 'none'},
								_1: {
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'padding', _1: '0'},
									_1: {
										ctor: '::',
										_0: {ctor: '_Tuple2', _0: 'margin', _1: '0'},
										_1: {ctor: '[]'}
									}
								}
							}
						}
					}
				}
			}
		}),
	_1: {ctor: '[]'}
};
var _pablen$toasty$Toasty_Defaults$config = A2(
	_pablen$toasty$Toasty$delay,
	5000,
	A2(
		_pablen$toasty$Toasty$itemAttrs,
		_pablen$toasty$Toasty_Defaults$itemAttrs,
		A2(
			_pablen$toasty$Toasty$containerAttrs,
			_pablen$toasty$Toasty_Defaults$containerAttrs,
			A2(
				_pablen$toasty$Toasty$transitionInAttrs,
				_pablen$toasty$Toasty_Defaults$transitionInAttrs,
				A2(
					_pablen$toasty$Toasty$transitionOutAttrs,
					_pablen$toasty$Toasty_Defaults$transitionOutAttrs,
					A2(_pablen$toasty$Toasty$transitionOutDuration, 700, _pablen$toasty$Toasty$config))))));
var _pablen$toasty$Toasty_Defaults$Error = F2(
	function (a, b) {
		return {ctor: 'Error', _0: a, _1: b};
	});
var _pablen$toasty$Toasty_Defaults$Warning = F2(
	function (a, b) {
		return {ctor: 'Warning', _0: a, _1: b};
	});
var _pablen$toasty$Toasty_Defaults$Success = F2(
	function (a, b) {
		return {ctor: 'Success', _0: a, _1: b};
	});

var _surprisetalk$elm_bulma$Bulma_CDN$stylesheet = A3(
	_elm_lang$html$Html$node,
	'link',
	{
		ctor: '::',
		_0: _elm_lang$html$Html_Attributes$rel('stylesheet'),
		_1: {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$href('https://cdnjs.cloudflare.com/ajax/libs/bulma/0.6.2/css/bulma.min.css'),
			_1: {ctor: '[]'}
		}
	},
	{ctor: '[]'});

var _surprisetalk$elm_bulma$Helpers_ops = _surprisetalk$elm_bulma$Helpers_ops || {};
_surprisetalk$elm_bulma$Helpers_ops['+:'] = F2(
	function (xs, x) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			xs,
			{
				ctor: '::',
				_0: x,
				_1: {ctor: '[]'}
			});
	});
var _surprisetalk$elm_bulma$Helpers$mvalues = A3(
	_elm_lang$core$Basics$flip,
	_elm_lang$core$List$foldr,
	{ctor: '[]'},
	F2(
		function (x, xs) {
			var _p0 = x;
			if (_p0.ctor === 'Nothing') {
				return xs;
			} else {
				return {ctor: '::', _0: _p0._0, _1: xs};
			}
		}));
var _surprisetalk$elm_bulma$Helpers_ops = _surprisetalk$elm_bulma$Helpers_ops || {};
_surprisetalk$elm_bulma$Helpers_ops['*?'] = _elm_lang$core$Maybe$map;
var _surprisetalk$elm_bulma$Helpers$uncurry4 = F2(
	function (f, _p1) {
		var _p2 = _p1;
		return A4(f, _p2._0, _p2._1, _p2._2, _p2._3);
	});
var _surprisetalk$elm_bulma$Helpers$curry4 = F5(
	function (f, a, b, c, d) {
		return f(
			{ctor: '_Tuple4', _0: a, _1: b, _2: c, _3: d});
	});
var _surprisetalk$elm_bulma$Helpers$uncurry3 = F2(
	function (f, _p3) {
		var _p4 = _p3;
		return A3(f, _p4._0, _p4._1, _p4._2);
	});
var _surprisetalk$elm_bulma$Helpers$curry3 = F4(
	function (f, a, b, c) {
		return f(
			{ctor: '_Tuple3', _0: a, _1: b, _2: c});
	});
var _surprisetalk$elm_bulma$Helpers$fl = _elm_lang$core$Basics$flip;
var _surprisetalk$elm_bulma$Helpers_ops = _surprisetalk$elm_bulma$Helpers_ops || {};
_surprisetalk$elm_bulma$Helpers_ops['?.'] = _surprisetalk$elm_bulma$Helpers$fl(_elm_lang$core$Maybe$withDefault);
var _surprisetalk$elm_bulma$Helpers_ops = _surprisetalk$elm_bulma$Helpers_ops || {};
_surprisetalk$elm_bulma$Helpers_ops['?*'] = _surprisetalk$elm_bulma$Helpers$fl(
	F2(
		function (x, y) {
			return A2(_surprisetalk$elm_bulma$Helpers_ops['*?'], x, y);
		}));
var _surprisetalk$elm_bulma$Helpers$ls = A2(
	_surprisetalk$elm_bulma$Helpers$fl,
	F2(
		function (x, y) {
			return {ctor: '::', _0: x, _1: y};
		}),
	{ctor: '[]'});
var _surprisetalk$elm_bulma$Helpers$node = F4(
	function (tag, attrs_, classes, attrs) {
		return A2(
			_elm_lang$html$Html$node,
			tag,
			A2(
				F2(
					function (x, y) {
						return A2(_elm_lang$core$Basics_ops['++'], x, y);
					}),
				attrs,
				A2(
					F2(
						function (x, y) {
							return A2(_elm_lang$core$Basics_ops['++'], x, y);
						}),
					attrs_,
					_surprisetalk$elm_bulma$Helpers$ls(
						_elm_lang$html$Html_Attributes$class(
							A2(
								_elm_lang$core$String$join,
								' ',
								A2(
									_elm_lang$core$List$filter,
									F2(
										function (x, y) {
											return !_elm_lang$core$Native_Utils.eq(x, y);
										})(''),
									classes)))))));
	});
var _surprisetalk$elm_bulma$Helpers$y = _elm_lang$core$Basics$always;
var _surprisetalk$elm_bulma$Helpers$setFirst = function (_p5) {
	return _elm_lang$core$Tuple$mapFirst(
		_surprisetalk$elm_bulma$Helpers$y(_p5));
};
var _surprisetalk$elm_bulma$Helpers$setSecond = function (_p6) {
	return _elm_lang$core$Tuple$mapSecond(
		_surprisetalk$elm_bulma$Helpers$y(_p6));
};
var _surprisetalk$elm_bulma$Helpers_ops = _surprisetalk$elm_bulma$Helpers_ops || {};
_surprisetalk$elm_bulma$Helpers_ops['>>>'] = F2(
	function (x, y) {
		return A2(_surprisetalk$elm_bulma$Helpers_ops['>>>'], x, y);
	});
var _surprisetalk$elm_bulma$Helpers_ops = _surprisetalk$elm_bulma$Helpers_ops || {};
_surprisetalk$elm_bulma$Helpers_ops['<<<'] = F2(
	function (x, y) {
		return A2(_surprisetalk$elm_bulma$Helpers_ops['<<<'], x, y);
	});
var _surprisetalk$elm_bulma$Helpers_ops = _surprisetalk$elm_bulma$Helpers_ops || {};
_surprisetalk$elm_bulma$Helpers_ops['=>'] = F2(
	function (v0, v1) {
		return {ctor: '_Tuple2', _0: v0, _1: v1};
	});

var _surprisetalk$elm_bulma$Bulma_Modifiers$displayByDevice = function (_p0) {
	var _p1 = _p0;
	return _elm_lang$html$Html_Attributes$class(
		A2(
			_elm_lang$core$String$join,
			' ',
			{
				ctor: '::',
				_0: function () {
					var _p2 = _p1.mobile;
					switch (_p2.ctor) {
						case 'Block':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isBlock.mobile;
						case 'Flex':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isFlex.mobile;
						case 'Inline':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInline.mobile;
						case 'InlineBlock':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInlineBlock.mobile;
						case 'InlineFlex':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInlineFlex.mobile;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.visibility.isHidden.mobile;
					}
				}(),
				_1: {
					ctor: '::',
					_0: function () {
						var _p3 = _p1.tablet;
						switch (_p3.ctor) {
							case 'Block':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isBlock.tabletOnly;
							case 'Flex':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isFlex.tabletOnly;
							case 'Inline':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInline.tabletOnly;
							case 'InlineBlock':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInlineBlock.tabletOnly;
							case 'InlineFlex':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInlineFlex.tabletOnly;
							default:
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.visibility.isHidden.tabletOnly;
						}
					}(),
					_1: {
						ctor: '::',
						_0: function () {
							var _p4 = _p1.desktop;
							switch (_p4.ctor) {
								case 'Block':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isBlock.desktopOnly;
								case 'Flex':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isFlex.desktopOnly;
								case 'Inline':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInline.desktopOnly;
								case 'InlineBlock':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInlineBlock.desktopOnly;
								case 'InlineFlex':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInlineFlex.desktopOnly;
								default:
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.visibility.isHidden.desktopOnly;
							}
						}(),
						_1: {
							ctor: '::',
							_0: function () {
								var _p5 = _p1.widescreen;
								switch (_p5.ctor) {
									case 'Block':
										return 'is-block-widescreen-only';
									case 'Flex':
										return 'is-flex-widescreen-only';
									case 'Inline':
										return 'is-inline-widescreen-only';
									case 'InlineBlock':
										return 'is-inline-block-widescreen-only';
									case 'InlineFlex':
										return 'is-inline-flex-widescreen-only';
									default:
										return 'is-hidden-widescreen-only';
								}
							}(),
							_1: {
								ctor: '::',
								_0: function () {
									var _p6 = _p1.fullHD;
									switch (_p6.ctor) {
										case 'Block':
											return 'is-block-fullHD-only';
										case 'Flex':
											return 'is-flex-fullHD-only';
										case 'Inline':
											return 'is-inline-fullHD-only';
										case 'InlineBlock':
											return 'is-inline-block-fullHD-only';
										case 'InlineFlex':
											return 'is-inline-flex-fullHD-only';
										default:
											return 'is-hidden-fullHD';
									}
								}(),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}));
};
var _surprisetalk$elm_bulma$Bulma_Modifiers$display = function (d) {
	var _p7 = d;
	switch (_p7.ctor) {
		case 'Block':
			return _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isBlock.always);
		case 'Flex':
			return _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isFlex.always);
		case 'Inline':
			return _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInline.always);
		case 'InlineBlock':
			return _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInlineBlock.always);
		case 'InlineFlex':
			return _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.display.isInlineFlex.always);
		default:
			return _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.visibility.isHidden.always);
	}
};
var _surprisetalk$elm_bulma$Bulma_Modifiers$invisible = _elm_lang$html$Html_Attributes$class('is-invisible');
var _surprisetalk$elm_bulma$Bulma_Modifiers$shadowless = _elm_lang$html$Html_Attributes$class('is-shadowless');
var _surprisetalk$elm_bulma$Bulma_Modifiers$radiusless = _elm_lang$html$Html_Attributes$class('is-radiusless');
var _surprisetalk$elm_bulma$Bulma_Modifiers$clipped = _elm_lang$html$Html_Attributes$class('is-clipped');
var _surprisetalk$elm_bulma$Bulma_Modifiers$paddingless = _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.sizing.isPaddingless);
var _surprisetalk$elm_bulma$Bulma_Modifiers$marginless = _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.sizing.isMarginless);
var _surprisetalk$elm_bulma$Bulma_Modifiers$fullWidth = _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.sizing.isFullwidth);
var _surprisetalk$elm_bulma$Bulma_Modifiers$overlay = _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.sizing.isOverlay);
var _surprisetalk$elm_bulma$Bulma_Modifiers$unselectable = _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.interaction.isUnselectable);
var _surprisetalk$elm_bulma$Bulma_Modifiers$pulledRight = _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.$float.isPulledRight);
var _surprisetalk$elm_bulma$Bulma_Modifiers$pulledLeft = _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.$float.isPulledLeft);
var _surprisetalk$elm_bulma$Bulma_Modifiers$clearfix = _elm_lang$html$Html_Attributes$class(_danielnarey$elm_bulma_classes$BulmaClasses$bulma.properties.$float.isClearfix);
var _surprisetalk$elm_bulma$Bulma_Modifiers$Devices = F5(
	function (a, b, c, d, e) {
		return {mobile: a, tablet: b, desktop: c, widescreen: d, fullHD: e};
	});
var _surprisetalk$elm_bulma$Bulma_Modifiers$Large = {ctor: 'Large'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Medium = {ctor: 'Medium'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Standard = {ctor: 'Standard'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Small = {ctor: 'Small'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Width11 = {ctor: 'Width11'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Width10 = {ctor: 'Width10'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Width9 = {ctor: 'Width9'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Width8 = {ctor: 'Width8'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Width7 = {ctor: 'Width7'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Width6 = {ctor: 'Width6'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Width5 = {ctor: 'Width5'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Width4 = {ctor: 'Width4'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Width3 = {ctor: 'Width3'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Width2 = {ctor: 'Width2'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Width1 = {ctor: 'Width1'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Auto = {ctor: 'Auto'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Loading = {ctor: 'Loading'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Active = {ctor: 'Active'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Focus = {ctor: 'Focus'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Hover = {ctor: 'Hover'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Blur = {ctor: 'Blur'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Danger = {ctor: 'Danger'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Warning = {ctor: 'Warning'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Success = {ctor: 'Success'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Info = {ctor: 'Info'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Link = {ctor: 'Link'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Primary = {ctor: 'Primary'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Black = {ctor: 'Black'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Dark = {ctor: 'Dark'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Light = {ctor: 'Light'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$White = {ctor: 'White'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Default = {ctor: 'Default'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Right = {ctor: 'Right'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Centered = {ctor: 'Centered'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Left = {ctor: 'Left'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Bottom = {ctor: 'Bottom'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Top = {ctor: 'Top'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Down = {ctor: 'Down'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Up = {ctor: 'Up'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$InlineFlex = {ctor: 'InlineFlex'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$InlineBlock = {ctor: 'InlineBlock'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Inline = {ctor: 'Inline'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Hidden = {ctor: 'Hidden'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Flex = {ctor: 'Flex'};
var _surprisetalk$elm_bulma$Bulma_Modifiers$Block = {ctor: 'Block'};

var _surprisetalk$elm_bulma$Bulma_Elements$easyTitleWithSubtitle = F4(
	function (spacing, size, title, subtitle) {
		return {
			ctor: '::',
			_0: A5(
				_surprisetalk$elm_bulma$Helpers$node,
				'p',
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.title,
					_1: {
						ctor: '::',
						_0: function () {
							var _p0 = spacing;
							if (_p0 === true) {
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.spacing.isNormal;
							} else {
								return '';
							}
						}(),
						_1: {
							ctor: '::',
							_0: function () {
								var _p1 = size;
								switch (_p1.ctor) {
									case 'H1':
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is1;
									case 'H2':
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is2;
									case 'H3':
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is3;
									case 'H4':
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is4;
									case 'H5':
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is5;
									default:
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is6;
								}
							}(),
							_1: {ctor: '[]'}
						}
					}
				},
				{ctor: '[]'},
				title),
			_1: {
				ctor: '::',
				_0: A5(
					_surprisetalk$elm_bulma$Helpers$node,
					'p',
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.subtitle,
						_1: {
							ctor: '::',
							_0: function () {
								var _p2 = size;
								switch (_p2.ctor) {
									case 'H1':
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is3;
									case 'H2':
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is4;
									case 'H3':
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is5;
									default:
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is6;
								}
							}(),
							_1: {ctor: '[]'}
						}
					},
					{ctor: '[]'},
					subtitle),
				_1: {ctor: '[]'}
			}
		};
	});
var _surprisetalk$elm_bulma$Bulma_Elements$subtitle = function (size) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		function () {
			var _p3 = size;
			switch (_p3.ctor) {
				case 'H1':
					return 'h1';
				case 'H2':
					return 'h2';
				case 'H3':
					return 'h3';
				case 'H4':
					return 'h4';
				case 'H5':
					return 'h5';
				default:
					return 'h6';
			}
		}(),
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.subtitle,
			_1: {
				ctor: '::',
				_0: function () {
					var _p4 = size;
					switch (_p4.ctor) {
						case 'H1':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is1;
						case 'H2':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is2;
						case 'H3':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is3;
						case 'H4':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is4;
						case 'H5':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is5;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is6;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$title = function (size) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		function () {
			var _p5 = size;
			switch (_p5.ctor) {
				case 'H1':
					return 'h1';
				case 'H2':
					return 'h2';
				case 'H3':
					return 'h3';
				case 'H4':
					return 'h4';
				case 'H5':
					return 'h5';
				default:
					return 'h6';
			}
		}(),
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.title,
			_1: {
				ctor: '::',
				_0: function () {
					var _p6 = size;
					switch (_p6.ctor) {
						case 'H1':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is1;
						case 'H2':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is2;
						case 'H3':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is3;
						case 'H4':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is4;
						case 'H5':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is5;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.heading.size.is6;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$multitag = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: 'tags',
		_1: {
			ctor: '::',
			_0: 'has-addons',
			_1: {ctor: '[]'}
		}
	});
var _surprisetalk$elm_bulma$Bulma_Elements$tags = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: 'tags',
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Elements$deleteTag = function (_p7) {
	var _p8 = _p7;
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'a',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.ui,
			_1: {
				ctor: '::',
				_0: 'is-delete',
				_1: {
					ctor: '::',
					_0: function () {
						var _p9 = _p8.size;
						switch (_p9.ctor) {
							case 'Small':
								return '';
							case 'Standard':
								return '';
							case 'Medium':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.size.isMedium;
							default:
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.size.isLarge;
						}
					}(),
					_1: {
						ctor: '::',
						_0: function () {
							var _p10 = _p8.color;
							switch (_p10.ctor) {
								case 'Default':
									return '';
								case 'White':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isWhite;
								case 'Light':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isLight;
								case 'Dark':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isDark;
								case 'Black':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isBlack;
								case 'Primary':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isPrimary;
								case 'Link':
									return 'is-link';
								case 'Info':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isInfo;
								case 'Success':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isSuccess;
								case 'Warning':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isWarning;
								default:
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isDanger;
							}
						}(),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$tag = function (_p11) {
	var _p12 = _p11;
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		_p12.isLink ? 'a' : 'span',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.ui,
			_1: {
				ctor: '::',
				_0: function () {
					var _p13 = _p12.size;
					switch (_p13.ctor) {
						case 'Small':
							return '';
						case 'Standard':
							return '';
						case 'Medium':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.size.isMedium;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.size.isLarge;
					}
				}(),
				_1: {
					ctor: '::',
					_0: function () {
						var _p14 = _p12.color;
						switch (_p14.ctor) {
							case 'Default':
								return '';
							case 'White':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isWhite;
							case 'Light':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isLight;
							case 'Dark':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isDark;
							case 'Black':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isBlack;
							case 'Primary':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isPrimary;
							case 'Link':
								return 'is-link';
							case 'Info':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isInfo;
							case 'Success':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isSuccess;
							case 'Warning':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isWarning;
							default:
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tag.color.isDanger;
						}
					}(),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$roundedTag = F2(
	function (mods, attrs) {
		return A2(
			_surprisetalk$elm_bulma$Bulma_Elements$tag,
			mods,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('is-rounded'),
				_1: attrs
			});
	});
var _surprisetalk$elm_bulma$Bulma_Elements$easyRoundedTag = F2(
	function (mods, attrs) {
		return function (_p15) {
			return A3(
				_surprisetalk$elm_bulma$Bulma_Elements$roundedTag,
				mods,
				attrs,
				_surprisetalk$elm_bulma$Helpers$ls(
					_elm_lang$html$Html$text(_p15)));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Elements$easyTag = F2(
	function (mods, attrs) {
		return function (_p16) {
			return A3(
				_surprisetalk$elm_bulma$Bulma_Elements$tag,
				mods,
				attrs,
				_surprisetalk$elm_bulma$Helpers$ls(
					_elm_lang$html$Html$text(_p16)));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Elements$tagModifiers = {size: _surprisetalk$elm_bulma$Bulma_Modifiers$Standard, color: _surprisetalk$elm_bulma$Bulma_Modifiers$Default, isLink: false};
var _surprisetalk$elm_bulma$Bulma_Elements$tableCellHead = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'th',
	{ctor: '[]'},
	{ctor: '[]'});
var _surprisetalk$elm_bulma$Bulma_Elements$tableCell = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'td',
	{ctor: '[]'},
	{ctor: '[]'});
var _surprisetalk$elm_bulma$Bulma_Elements$tableRow = function (highlighted) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'tr',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: function () {
				var _p17 = highlighted;
				if (_p17 === true) {
					return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.table.row.state.isSelected;
				} else {
					return '';
				}
			}(),
			_1: {ctor: '[]'}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$tableFoot = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'tfoot',
	{ctor: '[]'},
	{ctor: '[]'});
var _surprisetalk$elm_bulma$Bulma_Elements$tableBody = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'tbody',
	{ctor: '[]'},
	{ctor: '[]'});
var _surprisetalk$elm_bulma$Bulma_Elements$tableHead = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'thead',
	{ctor: '[]'},
	{ctor: '[]'});
var _surprisetalk$elm_bulma$Bulma_Elements$table = function (_p18) {
	var _p19 = _p18;
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'table',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.table.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p20 = _p19.bordered;
					if (_p20 === true) {
						return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.table.style.isBordered;
					} else {
						return '';
					}
				}(),
				_1: {
					ctor: '::',
					_0: function () {
						var _p21 = _p19.striped;
						if (_p21 === true) {
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.table.style.isStriped;
						} else {
							return '';
						}
					}(),
					_1: {
						ctor: '::',
						_0: function () {
							var _p22 = _p19.narrow;
							if (_p22 === true) {
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.table.spacing.isNarrow;
							} else {
								return '';
							}
						}(),
						_1: {
							ctor: '::',
							_0: function () {
								var _p23 = _p19.hoverable;
								if (_p23 === true) {
									return 'is-hoverable';
								} else {
									return '';
								}
							}(),
							_1: {
								ctor: '::',
								_0: function () {
									var _p24 = _p19.fullWidth;
									if (_p24 === true) {
										return 'is-fullwidth';
									} else {
										return '';
									}
								}(),
								_1: {ctor: '[]'}
							}
						}
					}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$tableModifiers = {bordered: false, striped: false, narrow: false, hoverable: false, fullWidth: false};
var _surprisetalk$elm_bulma$Bulma_Elements$progress = function (_p25) {
	var _p26 = _p25;
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'progress',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.ui,
			_1: {
				ctor: '::',
				_0: function () {
					var _p27 = _p26.size;
					switch (_p27.ctor) {
						case 'Small':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.size.isSmall;
						case 'Standard':
							return '';
						case 'Medium':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.size.isMedium;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.size.isLarge;
					}
				}(),
				_1: {
					ctor: '::',
					_0: function () {
						var _p28 = _p26.color;
						switch (_p28.ctor) {
							case 'Default':
								return '';
							case 'White':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.color.isWhite;
							case 'Light':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.color.isLight;
							case 'Dark':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.color.isDark;
							case 'Black':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.color.isBlack;
							case 'Primary':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.color.isPrimary;
							case 'Link':
								return 'is-link';
							case 'Info':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.color.isInfo;
							case 'Success':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.color.isSuccess;
							case 'Warning':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.color.isWarning;
							default:
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.progress.color.isDanger;
						}
					}(),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$easyProgress = F3(
	function (mods, attrs, val) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Elements$progress,
			mods,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$value(
					_elm_lang$core$Basics$toString(
						_elm_lang$core$Basics$round(val * 100))),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$max('100'),
					_1: attrs
				}
			},
			{ctor: '[]'});
	});
var _surprisetalk$elm_bulma$Bulma_Elements$progressModifiers = {size: _surprisetalk$elm_bulma$Bulma_Modifiers$Standard, color: _surprisetalk$elm_bulma$Bulma_Modifiers$Default};
var _surprisetalk$elm_bulma$Bulma_Elements$notification = function (color) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.notification.ui,
			_1: {
				ctor: '::',
				_0: function () {
					var _p29 = color;
					switch (_p29.ctor) {
						case 'Default':
							return '';
						case 'White':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.notification.color.isWhite;
						case 'Light':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.notification.color.isLight;
						case 'Dark':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.notification.color.isDark;
						case 'Black':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.notification.color.isBlack;
						case 'Primary':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.notification.color.isPrimary;
						case 'Link':
							return 'is-link';
						case 'Info':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.notification.color.isInfo;
						case 'Success':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.notification.color.isSuccess;
						case 'Warning':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.notification.color.isWarning;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.notification.color.isDanger;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$image = function (shape) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'figure',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p30 = shape;
					switch (_p30.ctor) {
						case 'OneByOne':
							switch (_p30._0.ctor) {
								case 'Unbounded':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is1by1;
								case 'X16':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is16x16;
								case 'X24':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is24x24;
								case 'X32':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is32x32;
								case 'X48':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is48x48;
								case 'X64':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is64x64;
								case 'X96':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is96x96;
								default:
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is128x128;
							}
						case 'FourByThree':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is4by3;
						case 'ThreeByTwo':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is3by2;
						case 'SixteenByNine':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is16by9;
						case 'TwoByOne':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.image.size.is2by1;
						default:
							return '';
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$easyImage = F3(
	function (mods, attrs, src) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Elements$image,
			mods,
			attrs,
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$img,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$src(src),
						_1: {ctor: '[]'}
					},
					{ctor: '[]'}),
				_1: {ctor: '[]'}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Elements$easyPlaceholderImage = F2(
	function (shape, attrs) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Elements$image,
			shape,
			attrs,
			{
				ctor: '::',
				_0: A3(
					_elm_lang$core$Basics$flip,
					_elm_lang$html$Html$img,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$src(
							function (_p31) {
								var _p32 = _p31;
								return A2(
									_elm_lang$core$Basics_ops['++'],
									'http://bulma.io/images/placeholders/',
									A2(
										_elm_lang$core$Basics_ops['++'],
										_elm_lang$core$Basics$toString(_p32._0),
										A2(
											_elm_lang$core$Basics_ops['++'],
											'x',
											A2(
												_elm_lang$core$Basics_ops['++'],
												_elm_lang$core$Basics$toString(_p32._1),
												'.png'))));
							}(
								function () {
									var _p33 = shape;
									switch (_p33.ctor) {
										case 'OneByOne':
											switch (_p33._0.ctor) {
												case 'X16':
													return {ctor: '_Tuple2', _0: 16, _1: 16};
												case 'X24':
													return {ctor: '_Tuple2', _0: 24, _1: 24};
												case 'X32':
													return {ctor: '_Tuple2', _0: 32, _1: 32};
												case 'X48':
													return {ctor: '_Tuple2', _0: 48, _1: 48};
												case 'X64':
													return {ctor: '_Tuple2', _0: 64, _1: 64};
												case 'X96':
													return {ctor: '_Tuple2', _0: 96, _1: 96};
												case 'X128':
													return {ctor: '_Tuple2', _0: 128, _1: 128};
												default:
													return {ctor: '_Tuple2', _0: 256, _1: 256};
											}
										case 'FourByThree':
											return {ctor: '_Tuple2', _0: 640, _1: 480};
										case 'ThreeByTwo':
											return {ctor: '_Tuple2', _0: 480, _1: 320};
										case 'SixteenByNine':
											return {ctor: '_Tuple2', _0: 640, _1: 360};
										case 'TwoByOne':
											return {ctor: '_Tuple2', _0: 640, _1: 320};
										default:
											return {ctor: '_Tuple2', _0: 256, _1: 256};
									}
								}())),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Elements$icon = function (size) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'span',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.icon.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p34 = size;
					switch (_p34.ctor) {
						case 'Small':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.icon.size.isSmall;
						case 'Standard':
							return '';
						case 'Medium':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.icon.size.isMedium;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.icon.size.isLarge;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$delete = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'a',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.$delete.ui,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Elements$easyDelete = F2(
	function (attrs, msg) {
		return A2(
			_surprisetalk$elm_bulma$Bulma_Elements$delete,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onClick(msg),
				_1: attrs
			},
			{ctor: '[]'});
	});
var _surprisetalk$elm_bulma$Bulma_Elements$notificationWithDelete = F3(
	function (color, attrs, msg) {
		return function (_p35) {
			return A3(
				_surprisetalk$elm_bulma$Bulma_Elements$notification,
				color,
				attrs,
				A2(
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						}),
					A2(
						_surprisetalk$elm_bulma$Bulma_Elements$easyDelete,
						{ctor: '[]'},
						msg),
					_p35));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Elements$tagWithDelete = F3(
	function (mods, attrs, msg) {
		return function (_p36) {
			return A3(
				_surprisetalk$elm_bulma$Bulma_Elements$tag,
				mods,
				attrs,
				A3(
					_elm_lang$core$Basics$flip,
					F2(
						function (x, y) {
							return A2(_elm_lang$core$Basics_ops['++'], x, y);
						}),
					{
						ctor: '::',
						_0: A2(
							_surprisetalk$elm_bulma$Bulma_Elements$easyDelete,
							{ctor: '[]'},
							msg),
						_1: {ctor: '[]'}
					},
					_p36));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Elements$easyTagWithDelete = F4(
	function (mods, attrs, msg, str) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Elements$tag,
			mods,
			attrs,
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(str),
				_1: {
					ctor: '::',
					_0: A2(
						_surprisetalk$elm_bulma$Bulma_Elements$easyDelete,
						{ctor: '[]'},
						msg),
					_1: {ctor: '[]'}
				}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Elements$easyRoundedTagWithDelete = F4(
	function (mods, attrs, msg, str) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Elements$roundedTag,
			mods,
			attrs,
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(str),
				_1: {
					ctor: '::',
					_0: A2(
						_surprisetalk$elm_bulma$Bulma_Elements$easyDelete,
						{ctor: '[]'},
						msg),
					_1: {ctor: '[]'}
				}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Elements$content = function (size) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.content.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p37 = size;
					switch (_p37.ctor) {
						case 'Small':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.content.size.isSmall;
						case 'Standard':
							return '';
						case 'Medium':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.content.size.isMedium;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.content.size.isLarge;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$buttonModifiers = {disabled: false, outlined: false, inverted: false, rounded: false, $static: false, size: _surprisetalk$elm_bulma$Bulma_Modifiers$Standard, state: _surprisetalk$elm_bulma$Bulma_Modifiers$Blur, color: _surprisetalk$elm_bulma$Bulma_Modifiers$Default, iconLeft: _elm_lang$core$Maybe$Nothing, iconRight: _elm_lang$core$Maybe$Nothing};
var _surprisetalk$elm_bulma$Bulma_Elements$buttons = function (alignment) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: 'buttons',
			_1: {
				ctor: '::',
				_0: function () {
					var _p38 = alignment;
					switch (_p38.ctor) {
						case 'Left':
							return '';
						case 'Centered':
							return 'is-centered';
						default:
							return 'is-right';
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Elements$connectedButtons = function (alignment) {
	return function (_p39) {
		return A2(
			_surprisetalk$elm_bulma$Bulma_Elements$buttons,
			alignment,
			A2(
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				_elm_lang$html$Html_Attributes$class('has-addons'),
				_p39));
	};
};
var _surprisetalk$elm_bulma$Bulma_Elements$button = F3(
	function (_p40, attrs, htmls) {
		var _p41 = _p40;
		var iconRight_ = function () {
			var _p42 = _p41.iconRight;
			if (_p42.ctor === 'Just') {
				return {
					ctor: '::',
					_0: A3(
						_surprisetalk$elm_bulma$Bulma_Elements$icon,
						_p42._0._0,
						_p42._0._1,
						{
							ctor: '::',
							_0: _p42._0._2,
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			} else {
				return {ctor: '[]'};
			}
		}();
		var iconLeft_ = function () {
			var _p43 = _p41.iconLeft;
			if (_p43.ctor === 'Just') {
				return {
					ctor: '::',
					_0: A3(
						_surprisetalk$elm_bulma$Bulma_Elements$icon,
						_p43._0._0,
						_p43._0._1,
						{
							ctor: '::',
							_0: _p43._0._2,
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				};
			} else {
				return {ctor: '[]'};
			}
		}();
		var htmls_ = function () {
			var _p44 = htmls;
			if (_p44.ctor === '[]') {
				return A2(_elm_lang$core$Basics_ops['++'], iconLeft_, iconRight_);
			} else {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					iconLeft_,
					A2(
						_elm_lang$core$Basics_ops['++'],
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$span,
								{ctor: '[]'},
								_p44),
							_1: {ctor: '[]'}
						},
						iconRight_));
			}
		}();
		return A5(
			_surprisetalk$elm_bulma$Helpers$node,
			'button',
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$disabled(_p41.disabled),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.ui,
				_1: {
					ctor: '::',
					_0: function () {
						var _p45 = _p41.$static;
						if (_p45 === true) {
							return 'is-static';
						} else {
							return '';
						}
					}(),
					_1: {
						ctor: '::',
						_0: function () {
							var _p46 = _p41.outlined;
							if (_p46 === true) {
								return 'is-outlined';
							} else {
								return '';
							}
						}(),
						_1: {
							ctor: '::',
							_0: function () {
								var _p47 = _p41.inverted;
								if (_p47 === true) {
									return 'is-inverted';
								} else {
									return '';
								}
							}(),
							_1: {
								ctor: '::',
								_0: function () {
									var _p48 = _p41.rounded;
									if (_p48 === true) {
										return 'is-rounded';
									} else {
										return '';
									}
								}(),
								_1: {
									ctor: '::',
									_0: function () {
										var _p49 = _p41.color;
										switch (_p49.ctor) {
											case 'Default':
												return '';
											case 'White':
												return A2(_elm_lang$core$Basics_ops['++'], 'is-selected ', _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.color.isWhite);
											case 'Light':
												return A2(_elm_lang$core$Basics_ops['++'], 'is-selected ', _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.color.isLight);
											case 'Dark':
												return A2(_elm_lang$core$Basics_ops['++'], 'is-selected ', _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.color.isDark);
											case 'Black':
												return A2(_elm_lang$core$Basics_ops['++'], 'is-selected ', _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.color.isBlack);
											case 'Primary':
												return A2(_elm_lang$core$Basics_ops['++'], 'is-selected ', _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.color.isPrimary);
											case 'Link':
												return A2(_elm_lang$core$Basics_ops['++'], 'is-selected ', 'is-link');
											case 'Info':
												return A2(_elm_lang$core$Basics_ops['++'], 'is-selected ', _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.color.isInfo);
											case 'Success':
												return A2(_elm_lang$core$Basics_ops['++'], 'is-selected ', _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.color.isSuccess);
											case 'Warning':
												return A2(_elm_lang$core$Basics_ops['++'], 'is-selected ', _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.color.isWarning);
											default:
												return A2(_elm_lang$core$Basics_ops['++'], 'is-selected ', _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.color.isDanger);
										}
									}(),
									_1: {
										ctor: '::',
										_0: function () {
											var _p50 = _p41.size;
											switch (_p50.ctor) {
												case 'Small':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.size.isSmall;
												case 'Standard':
													return '';
												case 'Medium':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.size.isMedium;
												default:
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.button.size.isLarge;
											}
										}(),
										_1: {
											ctor: '::',
											_0: function () {
												var _p51 = _p41.state;
												switch (_p51.ctor) {
													case 'Blur':
														return '';
													case 'Hover':
														return 'is-hovered';
													case 'Focus':
														return 'is-focused';
													case 'Active':
														return 'is-active';
													default:
														return 'is-loading';
												}
											}(),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				}
			},
			attrs,
			htmls_);
	});
var _surprisetalk$elm_bulma$Bulma_Elements$easyButton = F4(
	function (mods, attrs, msg, str) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Elements$button,
			mods,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onClick(msg),
				_1: attrs
			},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text(str),
				_1: {ctor: '[]'}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Elements$box = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.box.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Elements$ButtonModifiers = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return {disabled: a, outlined: b, inverted: c, rounded: d, $static: e, size: f, state: g, color: h, iconLeft: i, iconRight: j};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _surprisetalk$elm_bulma$Bulma_Elements$ProgressModifiers = F2(
	function (a, b) {
		return {size: a, color: b};
	});
var _surprisetalk$elm_bulma$Bulma_Elements$TableModifiers = F5(
	function (a, b, c, d, e) {
		return {bordered: a, striped: b, narrow: c, hoverable: d, fullWidth: e};
	});
var _surprisetalk$elm_bulma$Bulma_Elements$TagModifiers = F3(
	function (a, b, c) {
		return {size: a, color: b, isLink: c};
	});
var _surprisetalk$elm_bulma$Bulma_Elements$TwoByOne = {ctor: 'TwoByOne'};
var _surprisetalk$elm_bulma$Bulma_Elements$SixteenByNine = {ctor: 'SixteenByNine'};
var _surprisetalk$elm_bulma$Bulma_Elements$ThreeByTwo = {ctor: 'ThreeByTwo'};
var _surprisetalk$elm_bulma$Bulma_Elements$FourByThree = {ctor: 'FourByThree'};
var _surprisetalk$elm_bulma$Bulma_Elements$OneByOne = function (a) {
	return {ctor: 'OneByOne', _0: a};
};
var _surprisetalk$elm_bulma$Bulma_Elements$Natural = {ctor: 'Natural'};
var _surprisetalk$elm_bulma$Bulma_Elements$Unbounded = {ctor: 'Unbounded'};
var _surprisetalk$elm_bulma$Bulma_Elements$X128 = {ctor: 'X128'};
var _surprisetalk$elm_bulma$Bulma_Elements$X96 = {ctor: 'X96'};
var _surprisetalk$elm_bulma$Bulma_Elements$X64 = {ctor: 'X64'};
var _surprisetalk$elm_bulma$Bulma_Elements$X48 = {ctor: 'X48'};
var _surprisetalk$elm_bulma$Bulma_Elements$X32 = {ctor: 'X32'};
var _surprisetalk$elm_bulma$Bulma_Elements$X24 = {ctor: 'X24'};
var _surprisetalk$elm_bulma$Bulma_Elements$X16 = {ctor: 'X16'};
var _surprisetalk$elm_bulma$Bulma_Elements$H6 = {ctor: 'H6'};
var _surprisetalk$elm_bulma$Bulma_Elements$H5 = {ctor: 'H5'};
var _surprisetalk$elm_bulma$Bulma_Elements$H4 = {ctor: 'H4'};
var _surprisetalk$elm_bulma$Bulma_Elements$H3 = {ctor: 'H3'};
var _surprisetalk$elm_bulma$Bulma_Elements$H2 = {ctor: 'H2'};
var _surprisetalk$elm_bulma$Bulma_Elements$H1 = {ctor: 'H1'};

var _surprisetalk$elm_bulma$Bulma_Components$tab = F4(
	function (active, attrs, attrs_, htmls) {
		return A2(
			_elm_lang$html$Html$li,
			A2(
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				function () {
					var _p0 = active;
					if (_p0 === true) {
						return _elm_lang$html$Html_Attributes$class('is-active');
					} else {
						return _elm_lang$html$Html_Attributes$class('');
					}
				}(),
				attrs),
			{
				ctor: '::',
				_0: A2(_elm_lang$html$Html$a, attrs_, htmls),
				_1: {ctor: '[]'}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Components$tabs = F3(
	function (_p1, attrs, attrs_) {
		var _p2 = _p1;
		return function (_p3) {
			return A5(
				_surprisetalk$elm_bulma$Helpers$node,
				'div',
				{ctor: '[]'},
				{
					ctor: '::',
					_0: 'tabs',
					_1: {
						ctor: '::',
						_0: function () {
							var _p4 = _p2.style;
							switch (_p4.ctor) {
								case 'Minimal':
									return '';
								case 'Boxed':
									return 'is-boxed';
								case 'Toggle':
									return 'is-toggle';
								default:
									return 'is-toggle is-toggle-rounded';
							}
						}(),
						_1: {
							ctor: '::',
							_0: function () {
								var _p5 = _p2.alignment;
								switch (_p5.ctor) {
									case 'Left':
										return '';
									case 'Centered':
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tabs.alignment.isCentered;
									default:
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tabs.alignment.isRight;
								}
							}(),
							_1: {
								ctor: '::',
								_0: function () {
									var _p6 = _p2.size;
									switch (_p6.ctor) {
										case 'Small':
											return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tabs.size.isSmall;
										case 'Standard':
											return '';
										case 'Medium':
											return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tabs.size.isMedium;
										default:
											return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tabs.size.isLarge;
									}
								}(),
								_1: {ctor: '[]'}
							}
						}
					}
				},
				attrs,
				_surprisetalk$elm_bulma$Helpers$ls(
					A2(_elm_lang$html$Html$ul, attrs_, _p3)));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Components$panelTab = function (active) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'a',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: function () {
				var _p7 = active;
				if (_p7 === true) {
					return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.panel.tabs.tab.state.isActive;
				} else {
					return '';
				}
			}(),
			_1: {ctor: '[]'}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$panelCheckbox = F4(
	function (active, attrs, attrs_, htmls) {
		return A2(
			_elm_lang$html$Html$a,
			{
				ctor: '::',
				_0: function () {
					var _p8 = active;
					if (_p8 === true) {
						return _elm_lang$html$Html_Attributes$class('panel-block is-active');
					} else {
						return _elm_lang$html$Html_Attributes$class('panel-block ');
					}
				}(),
				_1: attrs
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$input,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$type_('checkbox'),
						_1: attrs_
					},
					{ctor: '[]'}),
				_1: htmls
			});
	});
var _surprisetalk$elm_bulma$Bulma_Components$panelLabel = function (active) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'label',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.panel.block.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p9 = active;
					if (_p9 === true) {
						return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.panel.block.state.isActive;
					} else {
						return '';
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$panelLinkWithIcon = F5(
	function (active, attrs, attrs_, iconBodies, htmls) {
		return A2(
			_elm_lang$html$Html$a,
			{
				ctor: '::',
				_0: function () {
					var _p10 = active;
					if (_p10 === true) {
						return _elm_lang$html$Html_Attributes$class('panel-block is-active');
					} else {
						return _elm_lang$html$Html_Attributes$class('panel-block ');
					}
				}(),
				_1: attrs
			},
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$span,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('panel-icon'),
						_1: attrs_
					},
					iconBodies),
				_1: htmls
			});
	});
var _surprisetalk$elm_bulma$Bulma_Components$panelLink = function (active) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'a',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.panel.block.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p11 = active;
					if (_p11 === true) {
						return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.panel.block.state.isActive;
					} else {
						return '';
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$panelBlock = function (active) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.panel.block.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p12 = active;
					if (_p12 === true) {
						return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.panel.block.state.isActive;
					} else {
						return '';
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$panelTabs = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'p',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.panel.tabs.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$panelHeading = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'p',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.panel.heading,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$panel = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.panel.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$paginationEllipsis = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'span',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.pagination.list.ellipsis,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$easyPaginationEllipsis = function (attrs) {
	return A2(
		_surprisetalk$elm_bulma$Bulma_Components$paginationEllipsis,
		attrs,
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text('…'),
			_1: {ctor: '[]'}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$paginationLink = function (current) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'a',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.pagination.list.link.ui,
			_1: {
				ctor: '::',
				_0: function () {
					var _p13 = current;
					if (_p13 === true) {
						return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.pagination.list.link.state.isCurrent;
					} else {
						return '';
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$easyPaginationLink = F3(
	function (current, attrs, msg) {
		return function (_p14) {
			return A3(
				_surprisetalk$elm_bulma$Bulma_Components$paginationLink,
				current,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Events$onClick(msg),
					_1: attrs
				},
				_surprisetalk$elm_bulma$Helpers$ls(
					_elm_lang$html$Html$text(
						_elm_lang$core$Basics$toString(_p14))));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Components$paginationList = function (attrs) {
	return function (_p15) {
		return A5(
			_surprisetalk$elm_bulma$Helpers$node,
			'ul',
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.pagination.list.container,
				_1: {ctor: '[]'}
			},
			attrs,
			A2(
				_elm_lang$core$List$map,
				function (_p16) {
					return A2(
						_elm_lang$html$Html$li,
						{ctor: '[]'},
						_surprisetalk$elm_bulma$Helpers$ls(_p16));
				},
				_p15));
	};
};
var _surprisetalk$elm_bulma$Bulma_Components$paginationNext = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'a',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.pagination.next,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$easyPaginationNext = F2(
	function (attrs, msg) {
		return function (_p17) {
			return A2(
				_surprisetalk$elm_bulma$Bulma_Components$paginationNext,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Events$onClick(msg),
					_1: attrs
				},
				_surprisetalk$elm_bulma$Helpers$ls(
					_elm_lang$html$Html$text(_p17)));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Components$paginationPrev = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'a',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.pagination.previous,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$easyPaginationPrev = F2(
	function (attrs, msg) {
		return function (_p18) {
			return A2(
				_surprisetalk$elm_bulma$Bulma_Components$paginationPrev,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Events$onClick(msg),
					_1: attrs
				},
				_surprisetalk$elm_bulma$Helpers$ls(
					_elm_lang$html$Html$text(_p18)));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Components$pagination = function (alignment) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.pagination.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p19 = alignment;
					switch (_p19.ctor) {
						case 'Left':
							return '';
						case 'Centered':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.pagination.position.isCentered;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.pagination.position.isRight;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$roundedPagination = function (alignment) {
	return function (_p20) {
		return A2(
			_surprisetalk$elm_bulma$Bulma_Components$pagination,
			alignment,
			A2(
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				_elm_lang$html$Html_Attributes$class('is-rounded'),
				_p20));
	};
};
var _surprisetalk$elm_bulma$Bulma_Components$navbarDivider = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'hr',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: 'navbar-divider',
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$navbarDropdown = F2(
	function (isBoxed, alignment) {
		return A3(
			_surprisetalk$elm_bulma$Helpers$node,
			'div',
			{ctor: '[]'},
			{
				ctor: '::',
				_0: 'navbar-dropdown',
				_1: {
					ctor: '::',
					_0: function () {
						var _p21 = isBoxed;
						if (_p21 === true) {
							return 'is-boxed';
						} else {
							return '';
						}
					}(),
					_1: {
						ctor: '::',
						_0: function () {
							var _p22 = alignment;
							if (_p22.ctor === 'Right') {
								return 'is-right';
							} else {
								return '';
							}
						}(),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Components$navbarLink = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'a',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: 'navbar-link',
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$hoverableNavbarItemDropdown = F4(
	function (dir, attrs, link, dropdowns) {
		return A5(
			_surprisetalk$elm_bulma$Helpers$node,
			'div',
			{ctor: '[]'},
			{
				ctor: '::',
				_0: 'navbar-item',
				_1: {
					ctor: '::',
					_0: 'is-hoverable',
					_1: {
						ctor: '::',
						_0: function () {
							var _p23 = dir;
							if (_p23.ctor === 'Up') {
								return 'has-dropdown has-dropdown-up';
							} else {
								return 'has-dropdown';
							}
						}(),
						_1: {ctor: '[]'}
					}
				}
			},
			attrs,
			{ctor: '::', _0: link, _1: dropdowns});
	});
var _surprisetalk$elm_bulma$Bulma_Components$navbarItemDropdown = F5(
	function (isActive, dir, attrs, link, dropdowns) {
		return A5(
			_surprisetalk$elm_bulma$Helpers$node,
			'div',
			{ctor: '[]'},
			{
				ctor: '::',
				_0: 'navbar-item',
				_1: {
					ctor: '::',
					_0: function () {
						var _p24 = isActive;
						if (_p24 === true) {
							return 'is-active';
						} else {
							return '';
						}
					}(),
					_1: {
						ctor: '::',
						_0: function () {
							var _p25 = dir;
							if (_p25.ctor === 'Up') {
								return 'has-dropdown has-dropdown-up';
							} else {
								return 'has-dropdown';
							}
						}(),
						_1: {ctor: '[]'}
					}
				}
			},
			attrs,
			{ctor: '::', _0: link, _1: dropdowns});
	});
var _surprisetalk$elm_bulma$Bulma_Components$navbarItemLink = function (isActive) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'a',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: 'navbar-item',
			_1: {
				ctor: '::',
				_0: isActive ? 'is-active' : '',
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$navbarItem = function (isActive) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: 'navbar-item',
			_1: {
				ctor: '::',
				_0: isActive ? 'is-active' : '',
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$navbarEnd = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: 'navbar-end',
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$navbarStart = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: 'navbar-start',
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$navbarMenu = F2(
	function (isActive, attrs) {
		return A4(
			_surprisetalk$elm_bulma$Helpers$node,
			'div',
			{ctor: '[]'},
			{
				ctor: '::',
				_0: 'navbar-menu',
				_1: {
					ctor: '::',
					_0: isActive ? 'is-active' : '',
					_1: {ctor: '[]'}
				}
			},
			attrs);
	});
var _surprisetalk$elm_bulma$Bulma_Components$navbarBurger = function (isActive) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'a',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: 'navbar-burger',
			_1: {
				ctor: '::',
				_0: isActive ? 'is-active' : '',
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$navbarCross = function (attrs) {
	return A3(
		_surprisetalk$elm_bulma$Bulma_Components$navbarBurger,
		true,
		attrs,
		{ctor: '[]'});
};
var _surprisetalk$elm_bulma$Bulma_Components$navbarBrand = F3(
	function (attrs, burger, items) {
		return A2(
			_elm_lang$html$Html$div,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('navbar-brand'),
				_1: attrs
			},
			A2(
				_elm_lang$core$Basics_ops['++'],
				items,
				{
					ctor: '::',
					_0: burger,
					_1: {ctor: '[]'}
				}));
	});
var _surprisetalk$elm_bulma$Bulma_Components$navbar = function (_p26) {
	var _p27 = _p26;
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'nav',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: 'navbar',
			_1: {
				ctor: '::',
				_0: function () {
					var _p28 = _p27.transparent;
					if (_p28 === true) {
						return 'is-transparent';
					} else {
						return '';
					}
				}(),
				_1: {
					ctor: '::',
					_0: function () {
						var _p29 = _p27.color;
						switch (_p29.ctor) {
							case 'Default':
								return '';
							case 'White':
								return 'is-white';
							case 'Light':
								return 'is-light';
							case 'Dark':
								return 'is-dark';
							case 'Black':
								return 'is-black';
							case 'Primary':
								return 'is-primary';
							case 'Link':
								return 'is-link';
							case 'Info':
								return 'is-info';
							case 'Success':
								return 'is-success';
							case 'Warning':
								return 'is-warning';
							default:
								return 'is-danger';
						}
					}(),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$fixedNavbar = F2(
	function (dir, mods) {
		return function (_p30) {
			return A2(
				_surprisetalk$elm_bulma$Bulma_Components$navbar,
				mods,
				A2(
					F2(
						function (x, y) {
							return {ctor: '::', _0: x, _1: y};
						}),
					function () {
						var _p31 = dir;
						if (_p31.ctor === 'Top') {
							return _elm_lang$html$Html_Attributes$class('is-fixed-top');
						} else {
							return _elm_lang$html$Html_Attributes$class('is-fixed-bottom');
						}
					}(),
					_p30));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Components$navbarModifiers = {color: _surprisetalk$elm_bulma$Bulma_Modifiers$Default, transparent: false};
var _surprisetalk$elm_bulma$Bulma_Components$modalCardFoot = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.card.foot,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$modalCardBody = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.card.body,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$modalCardTitle = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.card.title,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$modalCardHead = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.card.head,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$modalCard = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.card.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$modalClose = function (size) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'button',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.close.ui,
			_1: {
				ctor: '::',
				_0: function () {
					var _p32 = size;
					switch (_p32.ctor) {
						case 'Small':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.close.size.isSmall;
						case 'Standard':
							return '';
						case 'Medium':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.close.size.isMedium;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.close.size.isLarge;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$easyModalClose = F3(
	function (size, attrs, onClickModal) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Components$modalClose,
			size,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onClick(onClickModal),
				_1: attrs
			},
			{ctor: '[]'});
	});
var _surprisetalk$elm_bulma$Bulma_Components$modalContent = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.content,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$modalBackground = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.background,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$easyModalBackground = F2(
	function (attrs, onClickBackground) {
		return A2(
			_surprisetalk$elm_bulma$Bulma_Components$modalBackground,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onClick(onClickBackground),
				_1: attrs
			},
			{ctor: '[]'});
	});
var _surprisetalk$elm_bulma$Bulma_Components$modal = function (active) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p33 = active;
					if (_p33 === true) {
						return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.modal.state.isActive;
					} else {
						return '';
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$easyModal = F4(
	function (active, attrs, close, content) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Components$modal,
			active,
			attrs,
			{
				ctor: '::',
				_0: A2(
					_surprisetalk$elm_bulma$Bulma_Components$easyModalBackground,
					{ctor: '[]'},
					close),
				_1: {
					ctor: '::',
					_0: A2(
						_surprisetalk$elm_bulma$Bulma_Components$modalContent,
						{ctor: '[]'},
						content),
					_1: {
						ctor: '::',
						_0: A3(
							_surprisetalk$elm_bulma$Bulma_Components$easyModalClose,
							_surprisetalk$elm_bulma$Bulma_Modifiers$Standard,
							{ctor: '[]'},
							close),
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Components$messageBody = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.body,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$messageHeaderWithDelete = F2(
	function (attrs, msg) {
		return function (_p34) {
			return A5(
				_surprisetalk$elm_bulma$Helpers$node,
				'div',
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.header,
					_1: {ctor: '[]'}
				},
				attrs,
				A3(
					_elm_lang$core$Basics$flip,
					F2(
						function (x, y) {
							return A2(_elm_lang$core$Basics_ops['++'], x, y);
						}),
					{
						ctor: '::',
						_0: A2(
							_surprisetalk$elm_bulma$Bulma_Elements$easyDelete,
							{ctor: '[]'},
							msg),
						_1: {ctor: '[]'}
					},
					_p34));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Components$messageHeader = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.header,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$message = function (_p35) {
	var _p36 = _p35;
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'article',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p37 = _p36.color;
					switch (_p37.ctor) {
						case 'Default':
							return '';
						case 'White':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.color.isWhite;
						case 'Light':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.color.isLight;
						case 'Dark':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.color.isDark;
						case 'Black':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.color.isBlack;
						case 'Primary':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.color.isPrimary;
						case 'Link':
							return 'is-link';
						case 'Info':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.color.isInfo;
						case 'Success':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.color.isSuccess;
						case 'Warning':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.color.isWarning;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.message.color.isDanger;
					}
				}(),
				_1: {
					ctor: '::',
					_0: function () {
						var _p38 = _p36.size;
						switch (_p38.ctor) {
							case 'Small':
								return 'is-small';
							case 'Standard':
								return '';
							case 'Medium':
								return 'is-medium';
							default:
								return 'is-large';
						}
					}(),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$messageModifiers = {color: _surprisetalk$elm_bulma$Bulma_Modifiers$Default, size: _surprisetalk$elm_bulma$Bulma_Modifiers$Standard};
var _surprisetalk$elm_bulma$Bulma_Components$menuListItemLink = function (active) {
	return function (_p39) {
		return _elm_lang$html$Html$a(
			A2(
				F2(
					function (x, y) {
						return {ctor: '::', _0: x, _1: y};
					}),
				_elm_lang$html$Html_Attributes$class(
					active ? 'is-active' : ''),
				_p39));
	};
};
var _surprisetalk$elm_bulma$Bulma_Components$easyMenuListItemLink = F5(
	function (active, attrs, msg, icon, str) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Components$menuListItemLink,
			active,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Events$onClick(msg),
				_1: attrs
			},
			{
				ctor: '::',
				_0: icon,
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html$text(str),
					_1: {ctor: '[]'}
				}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Components$menuListItem = _elm_lang$html$Html$li;
var _surprisetalk$elm_bulma$Bulma_Components$menuList = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'ul',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.menu.list,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$menuLabel = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'p',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.menu.label,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$menu = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'aside',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.menu.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$dropdownDivider = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'hr',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: 'dropdown-divider',
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$dropdownItem = function (isActive) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: 'dropdown-item',
			_1: {
				ctor: '::',
				_0: isActive ? 'is-active' : '',
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$dropdownItemLink = function (isActive) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'a',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: 'dropdown-item',
			_1: {
				ctor: '::',
				_0: isActive ? 'is-active' : '',
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$dropdownMenu = F3(
	function (attrs, attrs_, items) {
		return A2(
			_elm_lang$html$Html$div,
			A2(
				_elm_lang$core$Basics_ops['++'],
				attrs,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('dropdown-menu'),
					_1: {
						ctor: '::',
						_0: A2(_elm_lang$html$Html_Attributes$attribute, 'role', 'menu'),
						_1: {ctor: '[]'}
					}
				}),
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$class('dropdown-content'),
						_1: attrs_
					},
					items),
				_1: {ctor: '[]'}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Components$dropdownTrigger = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: 'dropdown-trigger',
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$hoverableDropdown = function (_p40) {
	var _p41 = _p40;
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: 'dropdown',
			_1: {
				ctor: '::',
				_0: 'is-hoverable',
				_1: {
					ctor: '::',
					_0: function () {
						var _p42 = _p41.horizontalAlignment;
						if (_p42.ctor === 'Right') {
							return 'is-right';
						} else {
							return '';
						}
					}(),
					_1: {
						ctor: '::',
						_0: function () {
							var _p43 = _p41.verticalDirection;
							if (_p43.ctor === 'Up') {
								return 'is-up';
							} else {
								return '';
							}
						}(),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Components$dropdown = F2(
	function (isActive, _p44) {
		var _p45 = _p44;
		return A3(
			_surprisetalk$elm_bulma$Helpers$node,
			'div',
			{ctor: '[]'},
			{
				ctor: '::',
				_0: 'dropdown',
				_1: {
					ctor: '::',
					_0: function () {
						var _p46 = isActive;
						if (_p46 === true) {
							return 'is-active';
						} else {
							return '';
						}
					}(),
					_1: {
						ctor: '::',
						_0: function () {
							var _p47 = _p45.horizontalAlignment;
							if (_p47.ctor === 'Right') {
								return 'is-right';
							} else {
								return '';
							}
						}(),
						_1: {
							ctor: '::',
							_0: function () {
								var _p48 = _p45.verticalDirection;
								if (_p48.ctor === 'Up') {
									return 'is-up';
								} else {
									return '';
								}
							}(),
							_1: {ctor: '[]'}
						}
					}
				}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Components$dropdownModifiers = {horizontalAlignment: _surprisetalk$elm_bulma$Bulma_Modifiers$Left, verticalDirection: _surprisetalk$elm_bulma$Bulma_Modifiers$Down};
var _surprisetalk$elm_bulma$Bulma_Components$cardFooterItemLink = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'a',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.card.footer.item,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$cardFooterItem = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'p',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.card.footer.item,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$cardFooter = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'footer',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.card.footer.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$cardContent = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.card.content,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$cardImage = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.card.image,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$cardIconLink = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'a',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.card.header.icon,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$cardIcon = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'p',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.card.header.icon,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$easyCardIconLink = F2(
	function (attrs, msg) {
		return function (_p49) {
			return A2(
				_surprisetalk$elm_bulma$Bulma_Components$cardIcon,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Events$onClick(msg),
					_1: attrs
				},
				_surprisetalk$elm_bulma$Helpers$ls(_p49));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Components$cardTitle = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'p',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.card.header.title,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$easyCardTitle = function (attrs) {
	return function (_p50) {
		return A2(
			_surprisetalk$elm_bulma$Bulma_Components$cardTitle,
			attrs,
			_surprisetalk$elm_bulma$Helpers$ls(
				_elm_lang$html$Html$text(_p50)));
	};
};
var _surprisetalk$elm_bulma$Bulma_Components$cardHeader = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'header',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.card.header.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$easyCardHeader = F2(
	function (attrs, _p51) {
		var _p52 = _p51;
		return A2(
			_surprisetalk$elm_bulma$Bulma_Components$cardHeader,
			attrs,
			{
				ctor: '::',
				_0: A2(
					_surprisetalk$elm_bulma$Bulma_Components$cardTitle,
					{ctor: '[]'},
					_p52.title),
				_1: {
					ctor: '::',
					_0: A2(
						_surprisetalk$elm_bulma$Bulma_Components$cardIconLink,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onClick(_p52.onClickIcon),
							_1: {ctor: '[]'}
						},
						_p52.icon),
					_1: {ctor: '[]'}
				}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Components$easierCardHeader = F2(
	function (attrs, _p53) {
		var _p54 = _p53;
		return A2(
			_surprisetalk$elm_bulma$Bulma_Components$cardHeader,
			attrs,
			{
				ctor: '::',
				_0: A2(
					_surprisetalk$elm_bulma$Bulma_Components$easyCardTitle,
					{ctor: '[]'},
					_p54.title),
				_1: {
					ctor: '::',
					_0: A3(
						_surprisetalk$elm_bulma$Bulma_Components$easyCardIconLink,
						{ctor: '[]'},
						_p54.onClickIcon,
						_p54.icon),
					_1: {ctor: '[]'}
				}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Components$card = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.card.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Components$crumblet = F3(
	function (isActive, attrs, attrs_) {
		return function (_p55) {
			return A2(
				_elm_lang$html$Html$li,
				isActive ? {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$class('is-active'),
					_1: attrs
				} : attrs,
				_surprisetalk$elm_bulma$Helpers$ls(
					A2(_elm_lang$html$Html$a, attrs_, _p55)));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Components$breadcrumb = F3(
	function (_p56, attrs, attrs_) {
		var _p57 = _p56;
		return function (_p58) {
			return A5(
				_surprisetalk$elm_bulma$Helpers$node,
				'nav',
				{
					ctor: '::',
					_0: A2(_elm_lang$html$Html_Attributes$attribute, 'aria-label', 'breadcrumb'),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: 'breadcrumb',
					_1: {
						ctor: '::',
						_0: function () {
							var _p59 = _p57.separator;
							switch (_p59.ctor) {
								case 'Slash':
									return '';
								case 'Arrow':
									return 'has-arrow-separator';
								case 'Bullet':
									return 'has-bullet-separator';
								case 'Dot':
									return 'has-dot-separator';
								default:
									return 'has-succeeds-separator';
							}
						}(),
						_1: {
							ctor: '::',
							_0: function () {
								var _p60 = _p57.size;
								switch (_p60.ctor) {
									case 'Small':
										return 'is-small';
									case 'Standard':
										return '';
									case 'Medium':
										return 'is-medium';
									default:
										return 'is-large';
								}
							}(),
							_1: {
								ctor: '::',
								_0: function () {
									var _p61 = _p57.alignment;
									switch (_p61.ctor) {
										case 'Left':
											return '';
										case 'Centered':
											return 'is-centered';
										default:
											return 'is-right';
									}
								}(),
								_1: {ctor: '[]'}
							}
						}
					}
				},
				attrs,
				_surprisetalk$elm_bulma$Helpers$ls(
					A2(_elm_lang$html$Html$ul, attrs_, _p58)));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Components$BreadcrumbModifiers = F3(
	function (a, b, c) {
		return {separator: a, alignment: b, size: c};
	});
var _surprisetalk$elm_bulma$Bulma_Components$DropdownModifiers = F2(
	function (a, b) {
		return {horizontalAlignment: a, verticalDirection: b};
	});
var _surprisetalk$elm_bulma$Bulma_Components$MessageModifiers = F2(
	function (a, b) {
		return {color: a, size: b};
	});
var _surprisetalk$elm_bulma$Bulma_Components$NavbarModifiers = F2(
	function (a, b) {
		return {color: a, transparent: b};
	});
var _surprisetalk$elm_bulma$Bulma_Components$TabsModifiers = F3(
	function (a, b, c) {
		return {style: a, alignment: b, size: c};
	});
var _surprisetalk$elm_bulma$Bulma_Components$Succeeds = {ctor: 'Succeeds'};
var _surprisetalk$elm_bulma$Bulma_Components$Dot = {ctor: 'Dot'};
var _surprisetalk$elm_bulma$Bulma_Components$Bullet = {ctor: 'Bullet'};
var _surprisetalk$elm_bulma$Bulma_Components$Arrow = {ctor: 'Arrow'};
var _surprisetalk$elm_bulma$Bulma_Components$Slash = {ctor: 'Slash'};
var _surprisetalk$elm_bulma$Bulma_Components$breadcrumbModifiers = {separator: _surprisetalk$elm_bulma$Bulma_Components$Slash, alignment: _surprisetalk$elm_bulma$Bulma_Modifiers$Left, size: _surprisetalk$elm_bulma$Bulma_Modifiers$Standard};
var _surprisetalk$elm_bulma$Bulma_Components$Round = {ctor: 'Round'};
var _surprisetalk$elm_bulma$Bulma_Components$Toggle = {ctor: 'Toggle'};
var _surprisetalk$elm_bulma$Bulma_Components$Boxed = {ctor: 'Boxed'};
var _surprisetalk$elm_bulma$Bulma_Components$Minimal = {ctor: 'Minimal'};
var _surprisetalk$elm_bulma$Bulma_Components$tabsModifiers = {style: _surprisetalk$elm_bulma$Bulma_Components$Minimal, alignment: _surprisetalk$elm_bulma$Bulma_Modifiers$Left, size: _surprisetalk$elm_bulma$Bulma_Modifiers$Standard};

var _surprisetalk$elm_bulma$Bulma_Form$help = function (color) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'p',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.help.ui,
			_1: {
				ctor: '::',
				_0: function () {
					var _p0 = color;
					switch (_p0.ctor) {
						case 'Default':
							return '';
						case 'White':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.help.color.isWhite;
						case 'Light':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.help.color.isLight;
						case 'Dark':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.help.color.isDark;
						case 'Black':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.help.color.isBlack;
						case 'Primary':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.help.color.isPrimary;
						case 'Link':
							return 'is-link';
						case 'Info':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.help.color.isInfo;
						case 'Success':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.help.color.isSuccess;
						case 'Warning':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.help.color.isWarning;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.help.color.isDanger;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Form$controlHelp = _surprisetalk$elm_bulma$Bulma_Form$help;
var _surprisetalk$elm_bulma$Bulma_Form$controlRadioButton = F6(
	function (disabled, checked, name, attrs, attrs_, htmls) {
		var labelAttrs = {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$disabled(disabled),
			_1: {ctor: '[]'}
		};
		var inputAttrs = {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$name(name),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$type_('radio'),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$disabled(disabled),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$checked(checked),
						_1: {ctor: '[]'}
					}
				}
			}
		};
		return A5(
			_surprisetalk$elm_bulma$Helpers$node,
			'label',
			labelAttrs,
			{
				ctor: '::',
				_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.radio.ui,
				_1: {ctor: '[]'}
			},
			attrs,
			{
				ctor: '::',
				_0: A5(
					_surprisetalk$elm_bulma$Helpers$node,
					'input',
					inputAttrs,
					{ctor: '[]'},
					attrs_,
					{ctor: '[]'}),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html$text(' '),
					_1: htmls
				}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlSelectModifiers = {size: _surprisetalk$elm_bulma$Bulma_Modifiers$Standard, state: _surprisetalk$elm_bulma$Bulma_Modifiers$Blur, color: _surprisetalk$elm_bulma$Bulma_Modifiers$Default, expanded: false, iconLeft: _elm_lang$core$Maybe$Nothing};
var _surprisetalk$elm_bulma$Bulma_Form$controlTextAreaModifiers = {size: _surprisetalk$elm_bulma$Bulma_Modifiers$Standard, state: _surprisetalk$elm_bulma$Bulma_Modifiers$Blur, color: _surprisetalk$elm_bulma$Bulma_Modifiers$Default, readonly: false, disabled: false};
var _surprisetalk$elm_bulma$Bulma_Form$controlInputModifiers = {size: _surprisetalk$elm_bulma$Bulma_Modifiers$Standard, state: _surprisetalk$elm_bulma$Bulma_Modifiers$Blur, color: _surprisetalk$elm_bulma$Bulma_Modifiers$Default, expanded: false, rounded: false, readonly: false, disabled: false, iconLeft: _elm_lang$core$Maybe$Nothing, iconRight: _elm_lang$core$Maybe$Nothing};
var _surprisetalk$elm_bulma$Bulma_Form$label = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'label',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.label.ui,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlLabel = _surprisetalk$elm_bulma$Bulma_Form$label;
var _surprisetalk$elm_bulma$Bulma_Form$control = F3(
	function (_p1, attrs, htmls) {
		var _p2 = _p1;
		var _p14 = _p2.loading;
		var _p13 = _p2.iconRight;
		var _p12 = _p2.iconLeft;
		return A5(
			_surprisetalk$elm_bulma$Helpers$node,
			'p',
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.control.container,
				_1: {
					ctor: '::',
					_0: function () {
						var _p3 = _p14;
						if (_p3.ctor === 'Just') {
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.control.state.isLoading;
						} else {
							return '';
						}
					}(),
					_1: {
						ctor: '::',
						_0: function () {
							var _p4 = _p14;
							_v3_3:
							do {
								if (_p4.ctor === 'Just') {
									switch (_p4._0.ctor) {
										case 'Small':
											return 'is-small';
										case 'Medium':
											return 'is-medium';
										case 'Large':
											return 'is-large';
										default:
											break _v3_3;
									}
								} else {
									break _v3_3;
								}
							} while(false);
							return '';
						}(),
						_1: {
							ctor: '::',
							_0: function () {
								var _p5 = _p2.expanded;
								if (_p5 === true) {
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.control.sizing.isExpanded;
								} else {
									return '';
								}
							}(),
							_1: {
								ctor: '::',
								_0: function () {
									var _p6 = _p12;
									if (_p6.ctor === 'Just') {
										return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.control.hasIcons.left;
									} else {
										return '';
									}
								}(),
								_1: {
									ctor: '::',
									_0: function () {
										var _p7 = _p13;
										if (_p7.ctor === 'Just') {
											return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.control.hasIcons.right;
										} else {
											return '';
										}
									}(),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			},
			attrs,
			A2(
				_elm_lang$core$Basics_ops['++'],
				htmls,
				A2(
					_elm_lang$core$Basics_ops['++'],
					A2(
						_elm_lang$core$Maybe$withDefault,
						{ctor: '[]'},
						A2(
							_elm_lang$core$Maybe$map,
							function (_p8) {
								var _p9 = _p8;
								return {
									ctor: '::',
									_0: A3(
										_surprisetalk$elm_bulma$Bulma_Elements$icon,
										_p9._0,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('is-left'),
											_1: _p9._1
										},
										{
											ctor: '::',
											_0: _p9._2,
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								};
							},
							_p12)),
					A2(
						_elm_lang$core$Basics_ops['++'],
						A2(
							_elm_lang$core$Maybe$withDefault,
							{ctor: '[]'},
							A2(
								_elm_lang$core$Maybe$map,
								function (_p10) {
									var _p11 = _p10;
									return {
										ctor: '::',
										_0: A3(
											_surprisetalk$elm_bulma$Bulma_Elements$icon,
											_p11._0,
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$class('is-right'),
												_1: _p11._1
											},
											{
												ctor: '::',
												_0: _p11._2,
												_1: {ctor: '[]'}
											}),
										_1: {ctor: '[]'}
									};
								},
								_p13)),
						{ctor: '[]'}))));
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlInput = F3(
	function (_p15, attrs, attrs_) {
		var _p16 = _p15;
		var _p26 = _p16.state;
		var _p25 = _p16.size;
		var _p24 = _p16.readonly;
		var _p23 = _p16.disabled;
		var controlMods = {
			expanded: _p16.expanded,
			iconLeft: _p16.iconLeft,
			iconRight: _p16.iconRight,
			loading: function () {
				var _p17 = _p26;
				if (_p17.ctor === 'Loading') {
					return _elm_lang$core$Maybe$Just(_p25);
				} else {
					return _elm_lang$core$Maybe$Nothing;
				}
			}()
		};
		return function (_p18) {
			return A3(
				_surprisetalk$elm_bulma$Bulma_Form$control,
				controlMods,
				attrs,
				_surprisetalk$elm_bulma$Helpers$ls(
					A5(
						_surprisetalk$elm_bulma$Helpers$node,
						'input',
						_p24 ? {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$disabled(_p23),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$readonly(_p24),
								_1: {ctor: '[]'}
							}
						} : {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$disabled(_p23),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.ui,
							_1: {
								ctor: '::',
								_0: function () {
									var _p19 = _p25;
									switch (_p19.ctor) {
										case 'Small':
											return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.size.isSmall;
										case 'Standard':
											return '';
										case 'Medium':
											return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.size.isMedium;
										default:
											return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.size.isLarge;
									}
								}(),
								_1: {
									ctor: '::',
									_0: function () {
										var _p20 = _p26;
										switch (_p20.ctor) {
											case 'Hover':
												return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.state.isHovered;
											case 'Focus':
												return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.state.isFocused;
											case 'Active':
												return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.state.isActive;
											case 'Loading':
												return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.state.isLoading;
											default:
												return '';
										}
									}(),
									_1: {
										ctor: '::',
										_0: function () {
											var _p21 = _p16.color;
											switch (_p21.ctor) {
												case 'Default':
													return '';
												case 'White':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isWhite;
												case 'Light':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isLight;
												case 'Dark':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isDark;
												case 'Black':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isBlack;
												case 'Primary':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isPrimary;
												case 'Link':
													return 'is-link';
												case 'Info':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isInfo;
												case 'Success':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isSuccess;
												case 'Warning':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isWarning;
												default:
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isDanger;
											}
										}(),
										_1: {
											ctor: '::',
											_0: function () {
												var _p22 = _p16.rounded;
												if (_p22 === false) {
													return '';
												} else {
													return 'is-rounded';
												}
											}(),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						},
						attrs_,
						_p18)));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlText = F3(
	function (mods, attrs, attrs_) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Form$controlInput,
			mods,
			attrs,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$type_('text'),
				_1: attrs_
			});
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlPassword = F3(
	function (mods, attrs, attrs_) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Form$controlInput,
			mods,
			attrs,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$type_('password'),
				_1: attrs_
			});
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlEmail = F3(
	function (mods, attrs, attrs_) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Form$controlInput,
			mods,
			attrs,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$type_('email'),
				_1: attrs_
			});
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlPhone = F3(
	function (mods, attrs, attrs_) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Form$controlInput,
			mods,
			attrs,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$type_('tel'),
				_1: attrs_
			});
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlTextArea = F3(
	function (_p27, attrs, attrs_) {
		var _p28 = _p27;
		var _p37 = _p28.state;
		var _p36 = _p28.size;
		var _p35 = _p28.readonly;
		var _p34 = _p28.disabled;
		var controlMods = {
			expanded: false,
			iconLeft: _elm_lang$core$Maybe$Nothing,
			iconRight: _elm_lang$core$Maybe$Nothing,
			loading: function () {
				var _p29 = _p37;
				if (_p29.ctor === 'Loading') {
					return _elm_lang$core$Maybe$Just(_p36);
				} else {
					return _elm_lang$core$Maybe$Nothing;
				}
			}()
		};
		return function (_p30) {
			return A3(
				_surprisetalk$elm_bulma$Bulma_Form$control,
				controlMods,
				attrs,
				_surprisetalk$elm_bulma$Helpers$ls(
					A5(
						_surprisetalk$elm_bulma$Helpers$node,
						'textarea',
						_p35 ? {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$disabled(_p34),
							_1: {
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$readonly(_p35),
								_1: {ctor: '[]'}
							}
						} : {
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$disabled(_p34),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.ui,
							_1: {
								ctor: '::',
								_0: function () {
									var _p31 = _p36;
									switch (_p31.ctor) {
										case 'Small':
											return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.size.isSmall;
										case 'Standard':
											return '';
										case 'Medium':
											return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.size.isMedium;
										default:
											return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.size.isLarge;
									}
								}(),
								_1: {
									ctor: '::',
									_0: function () {
										var _p32 = _p37;
										switch (_p32.ctor) {
											case 'Hover':
												return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.state.isHovered;
											case 'Focus':
												return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.state.isFocused;
											case 'Active':
												return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.state.isActive;
											case 'Loading':
												return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.state.isLoading;
											default:
												return '';
										}
									}(),
									_1: {
										ctor: '::',
										_0: function () {
											var _p33 = _p28.color;
											switch (_p33.ctor) {
												case 'Default':
													return '';
												case 'White':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.color.isWhite;
												case 'Light':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.color.isLight;
												case 'Dark':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.color.isDark;
												case 'Black':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.color.isBlack;
												case 'Primary':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.color.isPrimary;
												case 'Link':
													return 'is-link';
												case 'Info':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.color.isInfo;
												case 'Success':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.color.isSuccess;
												case 'Warning':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.color.isWarning;
												default:
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.textarea.color.isDanger;
											}
										}(),
										_1: {ctor: '[]'}
									}
								}
							}
						},
						attrs_,
						_p30)));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlSelect = F3(
	function (_p38, attrs, attrs_) {
		var _p39 = _p38;
		var _p48 = _p39.state;
		var _p47 = _p39.size;
		var _p46 = _p39.expanded;
		var controlMods = {
			expanded: _p46,
			iconLeft: _p39.iconLeft,
			iconRight: _elm_lang$core$Maybe$Nothing,
			loading: function () {
				var _p40 = _p48;
				if (_p40.ctor === 'Loading') {
					return _elm_lang$core$Maybe$Just(_p47);
				} else {
					return _elm_lang$core$Maybe$Nothing;
				}
			}()
		};
		return function (_p41) {
			return A3(
				_surprisetalk$elm_bulma$Bulma_Form$control,
				controlMods,
				attrs,
				_surprisetalk$elm_bulma$Helpers$ls(
					A5(
						_surprisetalk$elm_bulma$Helpers$node,
						'span',
						{ctor: '[]'},
						{
							ctor: '::',
							_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.select.ui,
							_1: {ctor: '[]'}
						},
						{ctor: '[]'},
						_surprisetalk$elm_bulma$Helpers$ls(
							A5(
								_surprisetalk$elm_bulma$Helpers$node,
								'select',
								{ctor: '[]'},
								{
									ctor: '::',
									_0: function () {
										var _p42 = _p47;
										switch (_p42.ctor) {
											case 'Small':
												return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.select.size.isSmall;
											case 'Standard':
												return '';
											case 'Medium':
												return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.select.size.isMedium;
											default:
												return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.select.size.isLarge;
										}
									}(),
									_1: {
										ctor: '::',
										_0: function () {
											var _p43 = _p48;
											switch (_p43.ctor) {
												case 'Hover':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.select.state.isHovered;
												case 'Focus':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.select.state.isFocused;
												case 'Active':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.select.state.isActive;
												case 'Loading':
													return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.select.state.isLoading;
												default:
													return '';
											}
										}(),
										_1: {
											ctor: '::',
											_0: function () {
												var _p44 = _p39.color;
												switch (_p44.ctor) {
													case 'Default':
														return '';
													case 'White':
														return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isWhite;
													case 'Light':
														return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isLight;
													case 'Dark':
														return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isDark;
													case 'Black':
														return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isBlack;
													case 'Primary':
														return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isPrimary;
													case 'Link':
														return 'is-link';
													case 'Info':
														return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isInfo;
													case 'Success':
														return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isSuccess;
													case 'Warning':
														return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isWarning;
													default:
														return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.input.color.isDanger;
												}
											}(),
											_1: {
												ctor: '::',
												_0: function () {
													var _p45 = _p46;
													if (_p45 === true) {
														return 'is-fullwidth';
													} else {
														return '';
													}
												}(),
												_1: {ctor: '[]'}
											}
										}
									}
								},
								attrs_,
								_p41)))));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlSelectRounded = F3(
	function (mods, attrs, attrs_) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Form$controlSelect,
			mods,
			attrs,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('is-rounded'),
				_1: attrs_
			});
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlMultiselect = F3(
	function (mods, attrs, attrs_) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Form$controlSelect,
			mods,
			attrs,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$class('is-multiple'),
				_1: attrs_
			});
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlModifiers = {loading: _elm_lang$core$Maybe$Nothing, expanded: false, iconLeft: _elm_lang$core$Maybe$Nothing, iconRight: _elm_lang$core$Maybe$Nothing};
var _surprisetalk$elm_bulma$Bulma_Form$controlCheckBox = F5(
	function (disabled, attrs, attrs_, attrs__, htmls) {
		return A3(
			_surprisetalk$elm_bulma$Bulma_Form$control,
			_surprisetalk$elm_bulma$Bulma_Form$controlModifiers,
			attrs,
			_surprisetalk$elm_bulma$Helpers$ls(
				A5(
					_surprisetalk$elm_bulma$Helpers$node,
					'label',
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$disabled(disabled),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: 'checkbox',
						_1: {ctor: '[]'}
					},
					attrs_,
					{
						ctor: '::',
						_0: A5(
							_surprisetalk$elm_bulma$Helpers$node,
							'input',
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$disabled(disabled),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$type_('checkbox'),
									_1: {ctor: '[]'}
								}
							},
							{ctor: '[]'},
							attrs__,
							{ctor: '[]'}),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html$text(' '),
							_1: htmls
						}
					})));
	});
var _surprisetalk$elm_bulma$Bulma_Form$controlRadio = _surprisetalk$elm_bulma$Bulma_Form$control(_surprisetalk$elm_bulma$Bulma_Form$controlModifiers);
var _surprisetalk$elm_bulma$Bulma_Form$controlButton = F3(
	function (mods, attrs, attrs_) {
		return function (_p49) {
			return A3(
				_surprisetalk$elm_bulma$Bulma_Form$control,
				_surprisetalk$elm_bulma$Bulma_Form$controlModifiers,
				attrs,
				_surprisetalk$elm_bulma$Helpers$ls(
					A3(_surprisetalk$elm_bulma$Bulma_Elements$button, mods, attrs_, _p49)));
		};
	});
var _surprisetalk$elm_bulma$Bulma_Form$fieldBody = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: 'field-body',
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Form$fieldLabel = function (size) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.label,
			_1: {
				ctor: '::',
				_0: function () {
					var _p50 = size;
					switch (_p50.ctor) {
						case 'Small':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.label.size.isSmall;
						case 'Standard':
							return '';
						case 'Medium':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.label.size.isMedium;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.label.size.isLarge;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Form$horizontalFields = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.container,
		_1: {
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.layout.isHorizontal,
			_1: {ctor: '[]'}
		}
	});
var _surprisetalk$elm_bulma$Bulma_Form$multilineFields = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.container,
		_1: {
			ctor: '::',
			_0: 'is-grouped',
			_1: {
				ctor: '::',
				_0: 'is-grouped-multiline',
				_1: {ctor: '[]'}
			}
		}
	});
var _surprisetalk$elm_bulma$Bulma_Form$connectedFields = function (alignment) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p51 = alignment;
					switch (_p51.ctor) {
						case 'Left':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.hasAddons.left;
						case 'Centered':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.hasAddons.centered;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.hasAddons.right;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Form$fields = function (alignment) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p52 = alignment;
					switch (_p52.ctor) {
						case 'Left':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.isGrouped.left;
						case 'Centered':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.isGrouped.centered;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.isGrouped.right;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Form$field = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.field.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Form$ControlModifiers = F4(
	function (a, b, c, d) {
		return {loading: a, expanded: b, iconLeft: c, iconRight: d};
	});
var _surprisetalk$elm_bulma$Bulma_Form$ControlInputModifiers = F9(
	function (a, b, c, d, e, f, g, h, i) {
		return {size: a, state: b, color: c, expanded: d, rounded: e, readonly: f, disabled: g, iconLeft: h, iconRight: i};
	});
var _surprisetalk$elm_bulma$Bulma_Form$ControlTextAreaModifiers = F5(
	function (a, b, c, d, e) {
		return {size: a, state: b, color: c, readonly: d, disabled: e};
	});
var _surprisetalk$elm_bulma$Bulma_Form$ControlSelectModifiers = F5(
	function (a, b, c, d, e) {
		return {size: a, state: b, color: c, expanded: d, iconLeft: e};
	});

var _surprisetalk$elm_bulma$Bulma_Layout$tileChild = function (width) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.container,
			_1: {
				ctor: '::',
				_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.level.isChild,
				_1: {
					ctor: '::',
					_0: function () {
						var _p0 = width;
						switch (_p0.ctor) {
							case 'Auto':
								return '';
							case 'Width1':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is1;
							case 'Width2':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is2;
							case 'Width3':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is3;
							case 'Width4':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is4;
							case 'Width5':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is5;
							case 'Width6':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is6;
							case 'Width7':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is7;
							case 'Width8':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is8;
							case 'Width9':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is9;
							case 'Width10':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is10;
							default:
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is11;
						}
					}(),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Layout$verticalTileParent = function (width) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.container,
			_1: {
				ctor: '::',
				_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.level.isParent,
				_1: {
					ctor: '::',
					_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.orientation.isVertical,
					_1: {
						ctor: '::',
						_0: function () {
							var _p1 = width;
							switch (_p1.ctor) {
								case 'Auto':
									return '';
								case 'Width1':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is1;
								case 'Width2':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is2;
								case 'Width3':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is3;
								case 'Width4':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is4;
								case 'Width5':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is5;
								case 'Width6':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is6;
								case 'Width7':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is7;
								case 'Width8':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is8;
								case 'Width9':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is9;
								case 'Width10':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is10;
								default:
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is11;
							}
						}(),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Layout$tileParent = function (width) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.container,
			_1: {
				ctor: '::',
				_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.level.isParent,
				_1: {
					ctor: '::',
					_0: function () {
						var _p2 = width;
						switch (_p2.ctor) {
							case 'Auto':
								return '';
							case 'Width1':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is1;
							case 'Width2':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is2;
							case 'Width3':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is3;
							case 'Width4':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is4;
							case 'Width5':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is5;
							case 'Width6':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is6;
							case 'Width7':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is7;
							case 'Width8':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is8;
							case 'Width9':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is9;
							case 'Width10':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is10;
							default:
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is11;
						}
					}(),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Layout$tileAncestor = function (width) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.container,
			_1: {
				ctor: '::',
				_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.level.isAncestor,
				_1: {
					ctor: '::',
					_0: function () {
						var _p3 = width;
						switch (_p3.ctor) {
							case 'Auto':
								return '';
							case 'Width1':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is1;
							case 'Width2':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is2;
							case 'Width3':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is3;
							case 'Width4':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is4;
							case 'Width5':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is5;
							case 'Width6':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is6;
							case 'Width7':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is7;
							case 'Width8':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is8;
							case 'Width9':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is9;
							case 'Width10':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is10;
							default:
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is11;
						}
					}(),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Layout$verticalTile = function (width) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.container,
			_1: {
				ctor: '::',
				_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.orientation.isVertical,
				_1: {
					ctor: '::',
					_0: function () {
						var _p4 = width;
						switch (_p4.ctor) {
							case 'Auto':
								return '';
							case 'Width1':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is1;
							case 'Width2':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is2;
							case 'Width3':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is3;
							case 'Width4':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is4;
							case 'Width5':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is5;
							case 'Width6':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is6;
							case 'Width7':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is7;
							case 'Width8':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is8;
							case 'Width9':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is9;
							case 'Width10':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is10;
							default:
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is11;
						}
					}(),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Layout$tile = function (width) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'div',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p5 = width;
					switch (_p5.ctor) {
						case 'Auto':
							return '';
						case 'Width1':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is1;
						case 'Width2':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is2;
						case 'Width3':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is3;
						case 'Width4':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is4;
						case 'Width5':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is5;
						case 'Width6':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is6;
						case 'Width7':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is7;
						case 'Width8':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is8;
						case 'Width9':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is9;
						case 'Width10':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is10;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.tile.width.is11;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Layout$footer = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'footer',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.footer.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$section = function (spacing) {
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'section',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.section.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p6 = spacing;
					switch (_p6.ctor) {
						case 'NotSpaced':
							return '';
						case 'Spaced':
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.section.spacing.isMedium;
						default:
							return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.section.spacing.isLarge;
					}
				}(),
				_1: {ctor: '[]'}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Layout$heroFoot = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.foot,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$heroBody = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.body,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$heroHead = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.head,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$heroModifiers = {bold: false, size: _surprisetalk$elm_bulma$Bulma_Modifiers$Small, color: _surprisetalk$elm_bulma$Bulma_Modifiers$Default};
var _surprisetalk$elm_bulma$Bulma_Layout$hero = function (_p7) {
	var _p8 = _p7;
	return A3(
		_surprisetalk$elm_bulma$Helpers$node,
		'section',
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.container,
			_1: {
				ctor: '::',
				_0: function () {
					var _p9 = _p8.bold;
					if (_p9 === true) {
						return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.style.isBold;
					} else {
						return '';
					}
				}(),
				_1: {
					ctor: '::',
					_0: function () {
						var _p10 = _p8.size;
						switch (_p10.ctor) {
							case 'Small':
								return '';
							case 'Standard':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.size.isMedium;
							case 'Medium':
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.size.isLarge;
							default:
								return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.size.isFullheight;
						}
					}(),
					_1: {
						ctor: '::',
						_0: function () {
							var _p11 = _p8.color;
							switch (_p11.ctor) {
								case 'Default':
									return '';
								case 'White':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.color.isWhite;
								case 'Black':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.color.isBlack;
								case 'Light':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.color.isLight;
								case 'Dark':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.color.isDark;
								case 'Primary':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.color.isPrimary;
								case 'Link':
									return 'is-link';
								case 'Info':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.color.isInfo;
								case 'Success':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.color.isSuccess;
								case 'Warning':
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.color.isWarning;
								default:
									return _danielnarey$elm_bulma_classes$BulmaClasses$bulma.hero.color.isDanger;
							}
						}(),
						_1: {ctor: '[]'}
					}
				}
			}
		});
};
var _surprisetalk$elm_bulma$Bulma_Layout$easyHero = F3(
	function (mods, attrs, _p12) {
		var _p13 = _p12;
		return A3(
			_surprisetalk$elm_bulma$Bulma_Layout$hero,
			mods,
			attrs,
			{
				ctor: '::',
				_0: _p13.head,
				_1: {
					ctor: '::',
					_0: _p13.body,
					_1: {
						ctor: '::',
						_0: _p13.foot,
						_1: {ctor: '[]'}
					}
				}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Layout$mediaRight = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.media.right,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$mediaContent = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.media.content,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$mediaLeft = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.media.left,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$media = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'article',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.media.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$levelItemLink = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'a',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.level.item,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$levelItemText = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'p',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.level.item,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$levelItem = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.level.item,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$easyLevelItemWithHeading = F3(
	function (attrs, heading, title) {
		return A2(
			_surprisetalk$elm_bulma$Bulma_Layout$levelItem,
			attrs,
			{
				ctor: '::',
				_0: A2(
					_elm_lang$html$Html$div,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$p,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('heading'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: _elm_lang$html$Html$text(heading),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$p,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('title'),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: _elm_lang$html$Html$text(title),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}
					}),
				_1: {ctor: '[]'}
			});
	});
var _surprisetalk$elm_bulma$Bulma_Layout$levelRight = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.level.right,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$levelLeft = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.level.left,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$horizontalLevel = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'nav',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.level.container,
		_1: {
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.level.mobile.isHorizontal,
			_1: {ctor: '[]'}
		}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$level = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'nav',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.level.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$centeredLevel = _surprisetalk$elm_bulma$Bulma_Layout$level;
var _surprisetalk$elm_bulma$Bulma_Layout$fullHDContainer = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.feature.container,
		_1: {
			ctor: '::',
			_0: 'is-fullhd',
			_1: {ctor: '[]'}
		}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$widescreenContainer = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.feature.container,
		_1: {
			ctor: '::',
			_0: 'is-widescreen',
			_1: {ctor: '[]'}
		}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$fluidContainer = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.feature.container,
		_1: {
			ctor: '::',
			_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.feature.sizing.isFluid,
			_1: {ctor: '[]'}
		}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$container = A3(
	_surprisetalk$elm_bulma$Helpers$node,
	'div',
	{ctor: '[]'},
	{
		ctor: '::',
		_0: _danielnarey$elm_bulma_classes$BulmaClasses$bulma.feature.container,
		_1: {ctor: '[]'}
	});
var _surprisetalk$elm_bulma$Bulma_Layout$HeroModifiers = F3(
	function (a, b, c) {
		return {bold: a, size: b, color: c};
	});
var _surprisetalk$elm_bulma$Bulma_Layout$VerySpaced = {ctor: 'VerySpaced'};
var _surprisetalk$elm_bulma$Bulma_Layout$Spaced = {ctor: 'Spaced'};
var _surprisetalk$elm_bulma$Bulma_Layout$NotSpaced = {ctor: 'NotSpaced'};

var _user$project$Model$Model = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return function (l) {
												return function (m) {
													return {sections: a, startTime: b, currentSection: c, currentSectionTime: d, nextSection: e, sectionEditForm: f, remainingTime: g, targetTime: h, diffTime: i, scalars: j, toasties: k, lastUpdate: l, isResetModalOpen: m};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _user$project$Model$Section = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return function (k) {
											return {id: a, order: b, name: c, runner: d, carRoute: e, runnerRoute: f, proposition: g, plannedTime: h, realTime: i, timeToStart: j, timeToFinish: k};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var _user$project$Model$SectionEditForm = F4(
	function (a, b, c, d) {
		return {sectionId: a, plannedTime: b, realTime: c, isRealTimeValid: d};
	});
var _user$project$Model$Scalar = F3(
	function (a, b, c) {
		return {id: a, name: b, value: c};
	});
var _user$project$Model$RawDataFromServer = F2(
	function (a, b) {
		return {sections: a, scalars: b};
	});
var _user$project$Model$Readonly = {ctor: 'Readonly'};
var _user$project$Model$RealTime = {ctor: 'RealTime'};
var _user$project$Model$PlannedTime = {ctor: 'PlannedTime'};
var _user$project$Model$Reset = {ctor: 'Reset'};
var _user$project$Model$CloseResetDialog = {ctor: 'CloseResetDialog'};
var _user$project$Model$OpenResetDialog = {ctor: 'OpenResetDialog'};
var _user$project$Model$ToastyMsg = function (a) {
	return {ctor: 'ToastyMsg', _0: a};
};
var _user$project$Model$Reload = {ctor: 'Reload'};
var _user$project$Model$UpdateFromServer = function (a) {
	return {ctor: 'UpdateFromServer', _0: a};
};
var _user$project$Model$SectionUpdate = function (a) {
	return {ctor: 'SectionUpdate', _0: a};
};
var _user$project$Model$Tick = function (a) {
	return {ctor: 'Tick', _0: a};
};
var _user$project$Model$ValidateAndSaveEditForm = function (a) {
	return {ctor: 'ValidateAndSaveEditForm', _0: a};
};
var _user$project$Model$UpdateSectionFormItem = F2(
	function (a, b) {
		return {ctor: 'UpdateSectionFormItem', _0: a, _1: b};
	});
var _user$project$Model$StartSectionFormEdit = function (a) {
	return {ctor: 'StartSectionFormEdit', _0: a};
};
var _user$project$Model$Changeover = function (a) {
	return {ctor: 'Changeover', _0: a};
};
var _user$project$Model$ReceiveStartTime = function (a) {
	return {ctor: 'ReceiveStartTime', _0: a};
};
var _user$project$Model$Start = {ctor: 'Start'};

var _user$project$Utils$sectionStartTimeToString = function (timeToStart) {
	var _p0 = timeToStart;
	if (_p0.ctor === 'Just') {
		var dateValue = _elm_lang$core$Date$fromTime(_p0._0);
		return A2(
			_elm_lang$core$Basics_ops['++'],
			A3(
				_elm_lang$core$String$padLeft,
				2,
				_elm_lang$core$Native_Utils.chr(' '),
				_elm_lang$core$Basics$toString(
					_elm_lang$core$Date$hour(dateValue))),
			A2(
				_elm_lang$core$Basics_ops['++'],
				':',
				A2(
					_elm_lang$core$Basics_ops['++'],
					A3(
						_elm_lang$core$String$padLeft,
						2,
						_elm_lang$core$Native_Utils.chr('0'),
						_elm_lang$core$Basics$toString(
							_elm_lang$core$Date$minute(dateValue))),
					A2(
						_elm_lang$core$Basics_ops['++'],
						':',
						A3(
							_elm_lang$core$String$padLeft,
							2,
							_elm_lang$core$Native_Utils.chr('0'),
							_elm_lang$core$Basics$toString(
								_elm_lang$core$Date$second(dateValue)))))));
	} else {
		return '';
	}
};
var _user$project$Utils$addToast = F2(
	function (toast, _p1) {
		var _p2 = _p1;
		return A4(
			_pablen$toasty$Toasty$addToast,
			_pablen$toasty$Toasty_Defaults$config,
			_user$project$Model$ToastyMsg,
			toast,
			{ctor: '_Tuple2', _0: _p2._0, _1: _p2._1});
	});
var _user$project$Utils$getStartTimeScalarId = function (scalars) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		'-1',
		A2(
			_elm_lang$core$Maybe$andThen,
			function (s) {
				return _elm_lang$core$Maybe$Just(s.id);
			},
			A2(_elm_lang$core$Dict$get, 'startTime', scalars)));
};
var _user$project$Utils$getCurrentSectionScalarId = function (scalars) {
	return A2(
		_elm_lang$core$Maybe$withDefault,
		'-1',
		A2(
			_elm_lang$core$Maybe$andThen,
			function (s) {
				return _elm_lang$core$Maybe$Just(s.id);
			},
			A2(_elm_lang$core$Dict$get, 'currentSection', scalars)));
};
var _user$project$Utils$getScalarsDict = function (data) {
	return _elm_lang$core$Dict$fromList(
		A2(
			_elm_lang$core$List$map,
			function (scalar) {
				return {ctor: '_Tuple2', _0: scalar.name, _1: scalar};
			},
			data.scalars));
};
var _user$project$Utils$getRemainingTime = F2(
	function (sections, now) {
		var lastSection = _elm_lang$core$List$head(
			_elm_lang$core$List$reverse(sections));
		var remaining = function () {
			var _p3 = lastSection;
			if (_p3.ctor === 'Nothing') {
				return 0;
			} else {
				return A2(_elm_lang$core$Maybe$withDefault, 0, _p3._0.timeToFinish);
			}
		}();
		return remaining - now;
	});
var _user$project$Utils$getSection = F2(
	function (sections, section) {
		getSection:
		while (true) {
			var t = A2(
				_elm_lang$core$Maybe$withDefault,
				{ctor: '[]'},
				_elm_lang$core$List$tail(sections));
			var s = _elm_lang$core$List$head(sections);
			var _p4 = s;
			if (_p4.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p5 = _p4._0;
				if (_elm_lang$core$Native_Utils.eq(_p5.id, section.id)) {
					return _elm_lang$core$Maybe$Just(_p5);
				} else {
					var _v4 = t,
						_v5 = section;
					sections = _v4;
					section = _v5;
					continue getSection;
				}
			}
		}
	});
var _user$project$Utils$findNextSection = F2(
	function (section, sections) {
		findNextSection:
		while (true) {
			var t = A2(
				_elm_lang$core$Maybe$withDefault,
				{ctor: '[]'},
				_elm_lang$core$List$tail(sections));
			var s = _elm_lang$core$List$head(sections);
			var _p6 = s;
			if (_p6.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				if (_elm_lang$core$Native_Utils.eq(_p6._0.id, section.id)) {
					return _elm_lang$core$List$head(t);
				} else {
					var _v7 = section,
						_v8 = t;
					section = _v7;
					sections = _v8;
					continue findNextSection;
				}
			}
		}
	});
var _user$project$Utils$sectionById = F2(
	function (sectionId, sections) {
		sectionById:
		while (true) {
			var t = A2(
				_elm_lang$core$Maybe$withDefault,
				{ctor: '[]'},
				_elm_lang$core$List$tail(sections));
			var s = _elm_lang$core$List$head(sections);
			var _p7 = s;
			if (_p7.ctor === 'Nothing') {
				return _elm_lang$core$Maybe$Nothing;
			} else {
				var _p8 = _p7._0;
				if (_elm_lang$core$Native_Utils.eq(_p8.id, sectionId)) {
					return _elm_lang$core$Maybe$Just(_p8);
				} else {
					var _v10 = sectionId,
						_v11 = t;
					sectionId = _v10;
					sections = _v11;
					continue sectionById;
				}
			}
		}
	});
var _user$project$Utils$estimatedTime = function (section) {
	return A2(_elm_lang$core$Maybe$withDefault, section.plannedTime, section.realTime);
};
var _user$project$Utils$updateTimeToStart = F2(
	function (timeToStart, sections) {
		var remaining = A2(
			_elm_lang$core$Maybe$withDefault,
			{ctor: '[]'},
			_elm_lang$core$List$tail(sections));
		var section = A2(
			_elm_lang$core$Maybe$andThen,
			function (section) {
				var _p9 = timeToStart;
				if (_p9.ctor === 'Nothing') {
					return _elm_lang$core$Maybe$Just(
						_elm_lang$core$Native_Utils.update(
							section,
							{timeToStart: _elm_lang$core$Maybe$Nothing, timeToFinish: _elm_lang$core$Maybe$Nothing}));
				} else {
					var _p10 = _p9._0;
					return _elm_lang$core$Maybe$Just(
						_elm_lang$core$Native_Utils.update(
							section,
							{
								timeToStart: _elm_lang$core$Maybe$Just(_p10),
								timeToFinish: _elm_lang$core$Maybe$Just(
									_p10 + _user$project$Utils$estimatedTime(section))
							}));
				}
			},
			_elm_lang$core$List$head(sections));
		var _p11 = section;
		if (_p11.ctor === 'Nothing') {
			return {ctor: '[]'};
		} else {
			var _p12 = _p11._0;
			return A2(
				_elm_lang$core$Basics_ops['++'],
				{
					ctor: '::',
					_0: _p12,
					_1: {ctor: '[]'}
				},
				A2(_user$project$Utils$updateTimeToStart, _p12.timeToFinish, remaining));
		}
	});
var _user$project$Utils$getTargetTime = function (sections) {
	return A3(
		_elm_lang$core$List$foldr,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		A2(_elm_lang$core$List$map, _user$project$Utils$estimatedTime, sections));
};
var _user$project$Utils$getDiffComparedToPlan = function (sections) {
	var plannedTime = A3(
		_elm_lang$core$List$foldr,
		F2(
			function (x, y) {
				return x + y;
			}),
		0,
		A2(
			_elm_lang$core$List$map,
			function (section) {
				return section.plannedTime;
			},
			sections));
	var targetTime = _user$project$Utils$getTargetTime(sections);
	return targetTime - plannedTime;
};
var _user$project$Utils$timeDiffInSeconds = F2(
	function (a, b) {
		var timeB = _elm_lang$core$Date$toTime(b);
		var timeA = _elm_lang$core$Date$toTime(a);
		return (timeA - timeB) / 1000;
	});
var _user$project$Utils$emptySectionEditForm = A4(_user$project$Model$SectionEditForm, '', '', '', true);
var _user$project$Utils$updateSection = F2(
	function (sections, section) {
		var updateSection = function (s) {
			return _elm_lang$core$Native_Utils.eq(s.id, section.id) ? section : s;
		};
		return A2(_elm_lang$core$List$map, updateSection, sections);
	});
var _user$project$Utils$timeToString = function (time) {
	var timeInseconds = _elm_lang$core$Basics$abs(
		_elm_lang$core$Basics$round(time / 1000));
	var hours = (timeInseconds / 3600) | 0;
	var minutes = (A2(_elm_lang$core$Basics$rem, timeInseconds, 3600) / 60) | 0;
	var seconds = A2(_elm_lang$core$Basics$rem, timeInseconds, 60);
	return A2(
		_elm_lang$core$Basics_ops['++'],
		(_elm_lang$core$Native_Utils.cmp(time, 0) < 0) ? '-' : '',
		A2(
			_elm_lang$core$Basics_ops['++'],
			(_elm_lang$core$Native_Utils.cmp(hours, 0) > 0) ? A2(
				_elm_lang$core$Basics_ops['++'],
				A3(
					_elm_lang$core$String$padLeft,
					2,
					_elm_lang$core$Native_Utils.chr('0'),
					_elm_lang$core$Basics$toString(hours)),
				':') : '',
			A2(
				_elm_lang$core$Basics_ops['++'],
				A3(
					_elm_lang$core$String$padLeft,
					2,
					_elm_lang$core$Native_Utils.chr('0'),
					_elm_lang$core$Basics$toString(minutes)),
				A2(
					_elm_lang$core$Basics_ops['++'],
					':',
					A3(
						_elm_lang$core$String$padLeft,
						2,
						_elm_lang$core$Native_Utils.chr('0'),
						_elm_lang$core$Basics$toString(seconds))))));
};
var _user$project$Utils$maybeTimeToString = function (time) {
	var _p13 = time;
	if (_p13.ctor === 'Nothing') {
		return '';
	} else {
		return _user$project$Utils$timeToString(_p13._0);
	}
};
var _user$project$Utils$errorMessage = 'Invalid time format';
var _user$project$Utils$parseTimeUnit = function (value) {
	return A2(
		_elm_lang$core$Result$andThen,
		_elm_lang$core$String$toFloat,
		A2(_elm_lang$core$Result$fromMaybe, _user$project$Utils$errorMessage, value));
};
var _user$project$Utils$timeFromString = function (text) {
	var values = _elm_lang$core$Array$fromList(
		_elm_lang$core$List$reverse(
			A2(_elm_lang$core$String$split, ':', text)));
	var isSimpleFormat = _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(values),
		1) && _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$String$length(text),
		4);
	var seconds = A2(
		_elm_lang$core$Result$andThen,
		function (seconds) {
			return ((_elm_lang$core$Native_Utils.cmp(seconds, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(seconds, 60) < 0)) ? _elm_lang$core$Result$Ok(seconds) : _elm_lang$core$Result$Err(_user$project$Utils$errorMessage);
		},
		(!isSimpleFormat) ? _user$project$Utils$parseTimeUnit(
			A2(_elm_lang$core$Array$get, 0, values)) : _elm_lang$core$String$toFloat(
			A3(_elm_lang$core$String$slice, 2, 4, text)));
	var minutes = A2(
		_elm_lang$core$Result$andThen,
		function (minutes) {
			return ((_elm_lang$core$Native_Utils.cmp(minutes, 0) > -1) && (_elm_lang$core$Native_Utils.cmp(minutes, 60) < 0)) ? _elm_lang$core$Result$Ok(minutes) : _elm_lang$core$Result$Err(_user$project$Utils$errorMessage);
		},
		(!isSimpleFormat) ? _user$project$Utils$parseTimeUnit(
			A2(_elm_lang$core$Array$get, 1, values)) : _elm_lang$core$String$toFloat(
			A3(_elm_lang$core$String$slice, 0, 2, text)));
	var hours = ((!isSimpleFormat) && _elm_lang$core$Native_Utils.eq(
		_elm_lang$core$Array$length(values),
		3)) ? A2(
		_elm_lang$core$Result$andThen,
		function (hours) {
			return (_elm_lang$core$Native_Utils.cmp(hours, 0) > -1) ? _elm_lang$core$Result$Ok(hours) : _elm_lang$core$Result$Err(_user$project$Utils$errorMessage);
		},
		_user$project$Utils$parseTimeUnit(
			A2(_elm_lang$core$Array$get, 2, values))) : _elm_lang$core$Result$Ok(0);
	var result = A4(
		_elm_lang$core$Result$map3,
		F3(
			function (h, m, s) {
				return {h: h, m: m, s: s};
			}),
		hours,
		minutes,
		seconds);
	var _p14 = result;
	if (_p14.ctor === 'Ok') {
		var _p15 = _p14._0;
		return _elm_lang$core$Result$Ok(((_p15.h * _elm_lang$core$Time$hour) + (_p15.m * _elm_lang$core$Time$minute)) + (_p15.s * _elm_lang$core$Time$second));
	} else {
		return _elm_lang$core$Result$Err(_user$project$Utils$errorMessage);
	}
};
var _user$project$Utils$plannedTimeFromString = function (text) {
	var _p16 = _user$project$Utils$timeFromString(text);
	if (_p16.ctor === 'Ok') {
		return _p16._0;
	} else {
		return 0;
	}
};
var _user$project$Utils$realTimeFromString = function (text) {
	return _elm_lang$core$Result$toMaybe(
		_user$project$Utils$timeFromString(text));
};
var _user$project$Utils$validateAndUpdateSection = F2(
	function (section, sectionEditForm) {
		var realTimeValidation = function () {
			var _p17 = _user$project$Utils$timeFromString(sectionEditForm.realTime);
			if (_p17.ctor === 'Ok') {
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Maybe$Just(_p17._0),
					_1: true
				};
			} else {
				return {ctor: '_Tuple2', _0: section.realTime, _1: false};
			}
		}();
		var updatedForm = _elm_lang$core$Native_Utils.update(
			sectionEditForm,
			{
				sectionId: section.id,
				isRealTimeValid: _elm_lang$core$Tuple$second(realTimeValidation)
			});
		var updatedSection = _elm_lang$core$Native_Utils.update(
			section,
			{
				realTime: _elm_lang$core$Tuple$first(realTimeValidation)
			});
		var _p18 = _elm_lang$core$Tuple$second(realTimeValidation);
		if (_p18 === true) {
			return _elm_lang$core$Result$Ok(updatedSection);
		} else {
			return _elm_lang$core$Result$Err(updatedForm);
		}
	});

var _user$project$Server$toStringBody = function (string) {
	return A2(
		_elm_lang$http$Http$stringBody,
		'application/json',
		A3(_elm_community$string_extra$String_Extra$replace, '\n', '', string));
};
var _user$project$Server$scalarDecoder = A4(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional,
	'value',
	A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _elm_lang$core$Json_Decode$string),
	_elm_lang$core$Maybe$Nothing,
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
		'name',
		_elm_lang$core$Json_Decode$string,
		A3(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
			'id',
			_elm_lang$core$Json_Decode$string,
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$Model$Scalar))));
var _user$project$Server$sectionDecoder = A2(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$hardcoded,
	_elm_lang$core$Maybe$Nothing,
	A2(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$hardcoded,
		_elm_lang$core$Maybe$Nothing,
		A4(
			_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$optional,
			'realTime',
			A2(_elm_lang$core$Json_Decode$map, _elm_lang$core$Maybe$Just, _elm_lang$core$Json_Decode$float),
			_elm_lang$core$Maybe$Nothing,
			A3(
				_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
				'plannedTime',
				_elm_lang$core$Json_Decode$float,
				A3(
					_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
					'proposition',
					_elm_lang$core$Json_Decode$string,
					A3(
						_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
						'runnerRoute',
						_elm_lang$core$Json_Decode$string,
						A3(
							_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
							'carRoute',
							_elm_lang$core$Json_Decode$string,
							A3(
								_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
								'runner',
								_elm_lang$core$Json_Decode$string,
								A3(
									_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
									'name',
									_elm_lang$core$Json_Decode$string,
									A3(
										_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
										'order',
										_elm_lang$core$Json_Decode$int,
										A3(
											_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$required,
											'id',
											_elm_lang$core$Json_Decode$string,
											_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$Model$Section))))))))))));
var _user$project$Server$dataFromServerDecoder = A3(
	_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
	{
		ctor: '::',
		_0: 'data',
		_1: {
			ctor: '::',
			_0: 'allScalarses',
			_1: {ctor: '[]'}
		}
	},
	_elm_lang$core$Json_Decode$list(_user$project$Server$scalarDecoder),
	A3(
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$requiredAt,
		{
			ctor: '::',
			_0: 'data',
			_1: {
				ctor: '::',
				_0: 'allSections',
				_1: {ctor: '[]'}
			}
		},
		_elm_lang$core$Json_Decode$list(_user$project$Server$sectionDecoder),
		_NoRedInk$elm_decode_pipeline$Json_Decode_Pipeline$decode(_user$project$Model$RawDataFromServer)));
var _user$project$Server$sectionResetQuery = F2(
	function (index, section) {
		return A2(
			_elm_lang$core$Basics_ops['++'],
			'\n    reset',
			A2(
				_elm_lang$core$Basics_ops['++'],
				_elm_lang$core$Basics$toString(index),
				A2(
					_elm_lang$core$Basics_ops['++'],
					': updateSection(id: \\\"',
					A2(_elm_lang$core$Basics_ops['++'], section.id, '\\\"\n                            realTime: null\n                        ) {\n                            id\n                        }\n    '))));
	});
var _user$project$Server$apiServer = '/vltava/api';
var _user$project$Server$getSections = function () {
	var body = _user$project$Server$toStringBody('\n                {\n                    \"query\":\"query\n                    {\n                        allSections {\n                            id,\n                            order,\n                            name,\n                            carRoute,\n                            runnerRoute,\n                            proposition,\n                            runner,\n                            plannedTime,\n                            realTime },\n                        allScalarses {\n                            id,\n                            name,\n                            value}\n                        }\"\n                }');
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Model$UpdateFromServer,
		A3(_elm_lang$http$Http$post, _user$project$Server$apiServer, body, _user$project$Server$dataFromServerDecoder));
}();
var _user$project$Server$updateSection = function (section) {
	var realTime = function () {
		var _p0 = section.realTime;
		if (_p0.ctor === 'Just') {
			return _elm_lang$core$Basics$toString(_p0._0);
		} else {
			return 'null';
		}
	}();
	var body = _user$project$Server$toStringBody(
		A2(
			_elm_lang$core$Basics_ops['++'],
			'\n                {\n                    \"query\":\"mutation\n                    {\n                        updateSection(\n                            id: \\\"',
			A2(
				_elm_lang$core$Basics_ops['++'],
				section.id,
				A2(
					_elm_lang$core$Basics_ops['++'],
					'\\\"\n                            realTime: ',
					A2(_elm_lang$core$Basics_ops['++'], realTime, '\n                        ) { id  }\n                    }\"\n                }')))));
	return A2(
		_elm_lang$http$Http$send,
		_user$project$Model$SectionUpdate,
		A3(
			_elm_lang$http$Http$post,
			_user$project$Server$apiServer,
			body,
			A2(
				_elm_lang$core$Json_Decode$at,
				{
					ctor: '::',
					_0: 'data',
					_1: {
						ctor: '::',
						_0: 'updateSection',
						_1: {
							ctor: '::',
							_0: 'id',
							_1: {ctor: '[]'}
						}
					}
				},
				_elm_lang$core$Json_Decode$string)));
};
var _user$project$Server$changeover = F3(
	function (previousSection, currentSection, currentSectionScalarId) {
		var currentSectionId = function () {
			var _p1 = currentSection;
			if (_p1.ctor === 'Nothing') {
				return 'null';
			} else {
				return A2(
					_elm_lang$core$Basics_ops['++'],
					'\\\"',
					A2(_elm_lang$core$Basics_ops['++'], _p1._0.id, '\\\"'));
			}
		}();
		var realTime = function () {
			var _p2 = previousSection.realTime;
			if (_p2.ctor === 'Just') {
				return _elm_lang$core$Basics$toString(_p2._0);
			} else {
				return 'null';
			}
		}();
		var body = _user$project$Server$toStringBody(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'\n                {\n                    \"query\":\"mutation\n                    {\n                        updateSection: updateSection(\n                            id: \\\"',
				A2(
					_elm_lang$core$Basics_ops['++'],
					previousSection.id,
					A2(
						_elm_lang$core$Basics_ops['++'],
						'\\\"\n                            realTime: ',
						A2(
							_elm_lang$core$Basics_ops['++'],
							realTime,
							A2(
								_elm_lang$core$Basics_ops['++'],
								'\n                        ) {\n                            id\n                        },\n                        updateScalars: updateScalars(\n                            id: \\\"',
								A2(
									_elm_lang$core$Basics_ops['++'],
									currentSectionScalarId,
									A2(
										_elm_lang$core$Basics_ops['++'],
										'\\\"\n                            value: ',
										A2(_elm_lang$core$Basics_ops['++'], currentSectionId, '\n                        ) {\n                        id\n                    }\n                    }\"\n                }')))))))));
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Model$SectionUpdate,
			A3(
				_elm_lang$http$Http$post,
				_user$project$Server$apiServer,
				body,
				A2(
					_elm_lang$core$Json_Decode$at,
					{
						ctor: '::',
						_0: 'data',
						_1: {
							ctor: '::',
							_0: 'updateSection',
							_1: {
								ctor: '::',
								_0: 'id',
								_1: {ctor: '[]'}
							}
						}
					},
					_elm_lang$core$Json_Decode$string)));
	});
var _user$project$Server$start = F4(
	function (startTime, firstSection, startTimeScalarId, currentSectionScalarId) {
		var body = _user$project$Server$toStringBody(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'\n                {\n                    \"query\":\"mutation\n                    {\n                        updateStartTime: updateScalars(\n                            id: \\\"',
				A2(
					_elm_lang$core$Basics_ops['++'],
					startTimeScalarId,
					A2(
						_elm_lang$core$Basics_ops['++'],
						'\\\"\n                            value: \\\"',
						A2(
							_elm_lang$core$Basics_ops['++'],
							_elm_lang$core$Basics$toString(startTime),
							A2(
								_elm_lang$core$Basics_ops['++'],
								'\\\"\n                        ) {\n                            id\n                        },\n                        updateCurrentSection: updateScalars(\n                            id: \\\"',
								A2(
									_elm_lang$core$Basics_ops['++'],
									currentSectionScalarId,
									A2(
										_elm_lang$core$Basics_ops['++'],
										'\\\"\n                            value: \\\"',
										A2(_elm_lang$core$Basics_ops['++'], firstSection.id, '\\\"\n                        ) {\n                            id\n                        }\n                    }\"\n                }')))))))));
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Model$SectionUpdate,
			A3(
				_elm_lang$http$Http$post,
				_user$project$Server$apiServer,
				body,
				A2(
					_elm_lang$core$Json_Decode$at,
					{
						ctor: '::',
						_0: 'data',
						_1: {
							ctor: '::',
							_0: 'updateStartTime',
							_1: {
								ctor: '::',
								_0: 'id',
								_1: {ctor: '[]'}
							}
						}
					},
					_elm_lang$core$Json_Decode$string)));
	});
var _user$project$Server$reset = F3(
	function (sections, startTimeScalarId, currentSectionScalarId) {
		var indexes = A2(
			_elm_lang$core$List$range,
			1,
			_elm_lang$core$List$length(sections));
		var sectionQueries = A2(
			_elm_lang$core$String$join,
			' ',
			A3(_elm_lang$core$List$map2, _user$project$Server$sectionResetQuery, indexes, sections));
		var body = _user$project$Server$toStringBody(
			A2(
				_elm_lang$core$Basics_ops['++'],
				'\n                {\n                    \"query\":\"mutation\n                    { ',
				A2(
					_elm_lang$core$Basics_ops['++'],
					sectionQueries,
					A2(
						_elm_lang$core$Basics_ops['++'],
						'\n                        updateStartTime: updateScalars(\n                            id: \\\"',
						A2(
							_elm_lang$core$Basics_ops['++'],
							startTimeScalarId,
							A2(
								_elm_lang$core$Basics_ops['++'],
								'\\\"\n                            value: null\n                        ) {\n                            id\n                        },\n                        updateCurrentSection: updateScalars(\n                            id: \\\"',
								A2(_elm_lang$core$Basics_ops['++'], currentSectionScalarId, '\\\"\n                            value: null\n                        ) {\n                            id\n                        }\n                        }\"\n                    }')))))));
		return A2(
			_elm_lang$http$Http$send,
			_user$project$Model$SectionUpdate,
			A3(
				_elm_lang$http$Http$post,
				_user$project$Server$apiServer,
				body,
				A2(
					_elm_lang$core$Json_Decode$at,
					{
						ctor: '::',
						_0: 'data',
						_1: {
							ctor: '::',
							_0: 'updateStartTime',
							_1: {
								ctor: '::',
								_0: 'id',
								_1: {ctor: '[]'}
							}
						}
					},
					_elm_lang$core$Json_Decode$string)));
	});

var _user$project$Update$update = F2(
	function (msg, model) {
		var _p0 = msg;
		switch (_p0.ctor) {
			case 'Start':
				return {
					ctor: '_Tuple2',
					_0: model,
					_1: A2(_elm_lang$core$Task$perform, _user$project$Model$ReceiveStartTime, _elm_lang$core$Time$now)
				};
			case 'ReceiveStartTime':
				var startTimeScalarId = _user$project$Utils$getStartTimeScalarId(model.scalars);
				var currentSectionScalarId = _user$project$Utils$getCurrentSectionScalarId(model.scalars);
				var currentSection = _elm_lang$core$List$head(model.sections);
				var _p1 = currentSection;
				if (_p1.ctor === 'Just') {
					return {
						ctor: '_Tuple2',
						_0: model,
						_1: A4(_user$project$Server$start, _p0._0, _p1._0, startTimeScalarId, currentSectionScalarId)
					};
				} else {
					return A2(
						_user$project$Utils$addToast,
						A2(_pablen$toasty$Toasty_Defaults$Error, 'Nejde spustit závod, protože nemáš v pořádku načtená data ze serveru', 'Načti je tlačítkem \'Aktualizovat\''),
						{ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none});
				}
			case 'Changeover':
				var _p4 = _p0._0;
				var currentSectionScalarId = _user$project$Utils$getCurrentSectionScalarId(model.scalars);
				var currentSection = A2(_user$project$Utils$findNextSection, _p4, model.sections);
				var remainingTime = function () {
					var _p2 = currentSection;
					if (_p2.ctor === 'Nothing') {
						return _elm_lang$core$Maybe$Nothing;
					} else {
						return model.remainingTime;
					}
				}();
				var sectionEditForm = model.sectionEditForm;
				var realTime = _elm_lang$core$String$isEmpty(sectionEditForm.realTime) ? _user$project$Utils$maybeTimeToString(_p4.realTime) : sectionEditForm.realTime;
				var validationResult = A2(
					_user$project$Utils$validateAndUpdateSection,
					_p4,
					_elm_lang$core$Native_Utils.update(
						sectionEditForm,
						{realTime: realTime}));
				var _p3 = validationResult;
				if (_p3.ctor === 'Ok') {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{sectionEditForm: _user$project$Utils$emptySectionEditForm}),
						_1: A3(_user$project$Server$changeover, _p3._0, currentSection, currentSectionScalarId)
					};
				} else {
					return A2(
						_user$project$Utils$addToast,
						A2(_pablen$toasty$Toasty_Defaults$Error, 'Neplatný formát času', 'Čas musí být ve formátu \'mmss\', \'mm:ss\', nebo \'hh:mm:ss\''),
						{
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								model,
								{sectionEditForm: _p3._0}),
							_1: _elm_lang$core$Platform_Cmd$none
						});
				}
			case 'StartSectionFormEdit':
				var _p5 = _p0._0;
				var editForm = _elm_lang$core$Native_Utils.eq(model.sectionEditForm.sectionId, _p5.id) ? model.sectionEditForm : A4(
					_user$project$Model$SectionEditForm,
					_p5.id,
					_user$project$Utils$timeToString(_p5.plannedTime),
					_user$project$Utils$maybeTimeToString(_p5.realTime),
					true);
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{sectionEditForm: editForm}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'UpdateSectionFormItem':
				var _p7 = _p0._1;
				var currentForm = model.sectionEditForm;
				var updatedForm = function () {
					var _p6 = _p0._0;
					switch (_p6.ctor) {
						case 'PlannedTime':
							return _elm_lang$core$Native_Utils.update(
								currentForm,
								{plannedTime: _p7});
						case 'RealTime':
							return _elm_lang$core$Native_Utils.update(
								currentForm,
								{realTime: _p7});
						default:
							return currentForm;
					}
				}();
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{sectionEditForm: updatedForm}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'ValidateAndSaveEditForm':
				var validationResult = A2(_user$project$Utils$validateAndUpdateSection, _p0._0, model.sectionEditForm);
				var _p8 = validationResult;
				if (_p8.ctor === 'Ok') {
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{sectionEditForm: _user$project$Utils$emptySectionEditForm}),
						_1: _user$project$Server$updateSection(_p8._0)
					};
				} else {
					return A2(
						_user$project$Utils$addToast,
						A2(_pablen$toasty$Toasty_Defaults$Error, 'Neplatný formát času', 'Čas musí být ve formátu \'mmss\', \'mm:ss\', nebo \'hh:mm:ss\''),
						{
							ctor: '_Tuple2',
							_0: _elm_lang$core$Native_Utils.update(
								model,
								{sectionEditForm: _p8._0}),
							_1: _elm_lang$core$Platform_Cmd$none
						});
				}
			case 'Tick':
				var _p11 = _p0._0;
				var _p9 = model.startTime;
				if (_p9.ctor === 'Nothing') {
					return {ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none};
				} else {
					var isTimeToReload = (_elm_lang$core$Native_Utils.cmp(_p11 - model.lastUpdate, 30 * _elm_lang$core$Time$second) > 0) ? true : false;
					var currentSection = A2(
						_elm_lang$core$Maybe$andThen,
						_user$project$Utils$getSection(model.sections),
						model.currentSection);
					var currentSectionTime = A2(
						_elm_lang$core$Maybe$andThen,
						function (t) {
							return _elm_lang$core$Maybe$Just(t - _p11);
						},
						A2(
							_elm_lang$core$Maybe$andThen,
							function (_) {
								return _.timeToFinish;
							},
							currentSection));
					var remainingTime = function () {
						var _p10 = currentSection;
						if (_p10.ctor === 'Nothing') {
							return _elm_lang$core$Maybe$Nothing;
						} else {
							return _elm_lang$core$Maybe$Just(
								A2(_user$project$Utils$getRemainingTime, model.sections, _p11));
						}
					}();
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								currentSectionTime: currentSectionTime,
								remainingTime: remainingTime,
								lastUpdate: isTimeToReload ? _p11 : model.lastUpdate
							}),
						_1: isTimeToReload ? _user$project$Server$getSections : _elm_lang$core$Platform_Cmd$none
					};
				}
			case 'UpdateFromServer':
				var _p12 = A2(_elm_lang$core$Debug$log, 'data', _p0._0);
				if (_p12.ctor === 'Ok') {
					var _p14 = _p12._0;
					var scalars = _user$project$Utils$getScalarsDict(_p14);
					var startTime = A2(
						_elm_lang$core$Maybe$andThen,
						function (s) {
							return _elm_lang$core$Result$toMaybe(
								_elm_lang$core$String$toFloat(
									A2(_elm_lang$core$Maybe$withDefault, '', s.value)));
						},
						A2(_elm_lang$core$Dict$get, 'startTime', scalars));
					var sections = A2(
						_user$project$Utils$updateTimeToStart,
						startTime,
						A2(
							_elm_lang$core$List$sortBy,
							function (section) {
								return section.order;
							},
							_p14.sections));
					var currentSectionId = A2(
						_elm_lang$core$Maybe$andThen,
						function (s) {
							return s.value;
						},
						A2(_elm_lang$core$Dict$get, 'currentSection', scalars));
					var currentSection = A2(
						_elm_lang$core$Maybe$andThen,
						function (sectionId) {
							return A2(_user$project$Utils$sectionById, sectionId, sections);
						},
						currentSectionId);
					var nextSection = A2(
						_elm_lang$core$Maybe$andThen,
						function (section) {
							return A2(_user$project$Utils$findNextSection, section, sections);
						},
						currentSection);
					var remainingTime = function () {
						var _p13 = currentSection;
						if (_p13.ctor === 'Nothing') {
							return _elm_lang$core$Maybe$Nothing;
						} else {
							return model.remainingTime;
						}
					}();
					return {
						ctor: '_Tuple2',
						_0: _elm_lang$core$Native_Utils.update(
							model,
							{
								sections: sections,
								startTime: startTime,
								currentSection: currentSection,
								nextSection: nextSection,
								remainingTime: remainingTime,
								targetTime: _user$project$Utils$getTargetTime(sections),
								diffTime: _user$project$Utils$getDiffComparedToPlan(sections),
								scalars: scalars
							}),
						_1: _elm_lang$core$Platform_Cmd$none
					};
				} else {
					return A2(
						_user$project$Utils$addToast,
						A2(_pablen$toasty$Toasty_Defaults$Error, 'Nepodařilo se načíst data.', 'Možná jsi mimo signál. Zkus tlačítko \'Aktualizovat\' později'),
						{ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none});
				}
			case 'SectionUpdate':
				var _p15 = _p0._0;
				if (_p15.ctor === 'Err') {
					var m = A2(_elm_lang$core$Debug$log, 'Error', _p15._0);
					return A2(
						_user$project$Utils$addToast,
						A2(_pablen$toasty$Toasty_Defaults$Error, 'Nepodařilo se uložit změnu.', 'Možná jsi mimo signál. Zkus to později'),
						{ctor: '_Tuple2', _0: model, _1: _elm_lang$core$Platform_Cmd$none});
				} else {
					return A2(
						_user$project$Utils$addToast,
						A2(_pablen$toasty$Toasty_Defaults$Success, 'Změna byla v pořádku uložena.', ''),
						{ctor: '_Tuple2', _0: model, _1: _user$project$Server$getSections});
				}
			case 'Reload':
				return {ctor: '_Tuple2', _0: model, _1: _user$project$Server$getSections};
			case 'ToastyMsg':
				return A4(_pablen$toasty$Toasty$update, _pablen$toasty$Toasty$config, _user$project$Model$ToastyMsg, _p0._0, model);
			case 'OpenResetDialog':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{isResetModalOpen: true}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			case 'CloseResetDialog':
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{isResetModalOpen: false}),
					_1: _elm_lang$core$Platform_Cmd$none
				};
			default:
				var startTimeScalarId = _user$project$Utils$getStartTimeScalarId(model.scalars);
				var currentSectionScalarId = _user$project$Utils$getCurrentSectionScalarId(model.scalars);
				return {
					ctor: '_Tuple2',
					_0: _elm_lang$core$Native_Utils.update(
						model,
						{isResetModalOpen: false}),
					_1: A3(_user$project$Server$reset, model.sections, startTimeScalarId, currentSectionScalarId)
				};
		}
	});
var _user$project$Update$subscriptions = function (model) {
	var receIsOver = function () {
		var _p16 = model.currentSection;
		if (_p16.ctor === 'Nothing') {
			return true;
		} else {
			return false;
		}
	}();
	var receNotYetStarted = function () {
		var _p17 = model.startTime;
		if (_p17.ctor === 'Nothing') {
			return true;
		} else {
			return false;
		}
	}();
	return (receNotYetStarted || receIsOver) ? _elm_lang$core$Platform_Sub$none : A2(_elm_lang$core$Time$every, _elm_lang$core$Time$second, _user$project$Model$Tick);
};
var _user$project$Update$init = function () {
	var emptySections = {ctor: '[]'};
	return {
		ctor: '_Tuple2',
		_0: {
			sections: emptySections,
			startTime: _elm_lang$core$Maybe$Nothing,
			currentSection: _elm_lang$core$Maybe$Nothing,
			currentSectionTime: _elm_lang$core$Maybe$Nothing,
			sectionEditForm: _user$project$Utils$emptySectionEditForm,
			nextSection: _elm_lang$core$Maybe$Nothing,
			remainingTime: _elm_lang$core$Maybe$Nothing,
			targetTime: _user$project$Utils$getTargetTime(emptySections),
			diffTime: _user$project$Utils$getDiffComparedToPlan(emptySections),
			scalars: _elm_lang$core$Dict$empty,
			toasties: _pablen$toasty$Toasty$initialState,
			lastUpdate: 0,
			isResetModalOpen: false
		},
		_1: _user$project$Server$getSections
	};
}();

var _user$project$Main$resetModal = function (isOpened) {
	return A3(
		_surprisetalk$elm_bulma$Bulma_Components$modal,
		isOpened,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A2(
				_surprisetalk$elm_bulma$Bulma_Components$modalBackground,
				{ctor: '[]'},
				{ctor: '[]'}),
			_1: {
				ctor: '::',
				_0: A2(
					_surprisetalk$elm_bulma$Bulma_Components$modalContent,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A3(
							_surprisetalk$elm_bulma$Bulma_Components$message,
							_elm_lang$core$Native_Utils.update(
								_surprisetalk$elm_bulma$Bulma_Components$messageModifiers,
								{color: _surprisetalk$elm_bulma$Bulma_Modifiers$Warning}),
							{ctor: '[]'},
							{
								ctor: '::',
								_0: A2(
									_surprisetalk$elm_bulma$Bulma_Components$messageHeader,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text('Potvrzení'),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_surprisetalk$elm_bulma$Bulma_Components$messageBody,
										{ctor: '[]'},
										{
											ctor: '::',
											_0: A2(
												_elm_lang$html$Html$div,
												{
													ctor: '::',
													_0: _elm_lang$html$Html_Attributes$style(
														{
															ctor: '::',
															_0: {ctor: '_Tuple2', _0: 'padding', _1: '12px'},
															_1: {ctor: '[]'}
														}),
													_1: {ctor: '[]'}
												},
												{
													ctor: '::',
													_0: _elm_lang$html$Html$text('Opravdu chceš resetovat všechny zaznamenané časy od startu?'),
													_1: {ctor: '[]'}
												}),
											_1: {
												ctor: '::',
												_0: A2(
													_elm_lang$html$Html$div,
													{
														ctor: '::',
														_0: _elm_lang$html$Html_Attributes$style(
															{
																ctor: '::',
																_0: {ctor: '_Tuple2', _0: 'text-align', _1: 'center'},
																_1: {ctor: '[]'}
															}),
														_1: {ctor: '[]'}
													},
													{
														ctor: '::',
														_0: A3(
															_surprisetalk$elm_bulma$Bulma_Elements$button,
															_elm_lang$core$Native_Utils.update(
																_surprisetalk$elm_bulma$Bulma_Elements$buttonModifiers,
																{color: _surprisetalk$elm_bulma$Bulma_Modifiers$Danger}),
															{
																ctor: '::',
																_0: _elm_lang$html$Html_Events$onClick(_user$project$Model$Reset),
																_1: {ctor: '[]'}
															},
															{
																ctor: '::',
																_0: A2(
																	_elm_lang$html$Html$i,
																	{
																		ctor: '::',
																		_0: _elm_lang$html$Html_Attributes$class('fa fa-check'),
																		_1: {ctor: '[]'}
																	},
																	{ctor: '[]'}),
																_1: {
																	ctor: '::',
																	_0: _elm_lang$html$Html$text(' Jo, chci!'),
																	_1: {ctor: '[]'}
																}
															}),
														_1: {
															ctor: '::',
															_0: A3(
																_surprisetalk$elm_bulma$Bulma_Elements$button,
																_elm_lang$core$Native_Utils.update(
																	_surprisetalk$elm_bulma$Bulma_Elements$buttonModifiers,
																	{color: _surprisetalk$elm_bulma$Bulma_Modifiers$Success}),
																{
																	ctor: '::',
																	_0: _elm_lang$html$Html_Attributes$style(
																		{
																			ctor: '::',
																			_0: {ctor: '_Tuple2', _0: 'margin-left', _1: '8px'},
																			_1: {ctor: '[]'}
																		}),
																	_1: {
																		ctor: '::',
																		_0: _elm_lang$html$Html_Events$onClick(_user$project$Model$CloseResetDialog),
																		_1: {ctor: '[]'}
																	}
																},
																{
																	ctor: '::',
																	_0: A2(
																		_elm_lang$html$Html$i,
																		{
																			ctor: '::',
																			_0: _elm_lang$html$Html_Attributes$class('fa fa-ban'),
																			_1: {ctor: '[]'}
																		},
																		{ctor: '[]'}),
																	_1: {
																		ctor: '::',
																		_0: _elm_lang$html$Html$text(' Radši ne'),
																		_1: {ctor: '[]'}
																	}
																}),
															_1: {ctor: '[]'}
														}
													}),
												_1: {ctor: '[]'}
											}
										}),
									_1: {ctor: '[]'}
								}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A3(
						_surprisetalk$elm_bulma$Bulma_Components$modalClose,
						_surprisetalk$elm_bulma$Bulma_Modifiers$Large,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onClick(_user$project$Model$CloseResetDialog),
							_1: {ctor: '[]'}
						},
						{ctor: '[]'}),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Main$infoTag = function (msg) {
	return A3(
		_surprisetalk$elm_bulma$Bulma_Elements$tag,
		_elm_lang$core$Native_Utils.update(
			_surprisetalk$elm_bulma$Bulma_Elements$tagModifiers,
			{size: _surprisetalk$elm_bulma$Bulma_Modifiers$Medium, color: _surprisetalk$elm_bulma$Bulma_Modifiers$Light}),
		{
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$style(
				{
					ctor: '::',
					_0: {ctor: '_Tuple2', _0: 'font-weight', _1: 'bold'},
					_1: {ctor: '[]'}
				}),
			_1: {ctor: '[]'}
		},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text(msg),
			_1: {ctor: '[]'}
		});
};
var _user$project$Main$nothing = _elm_lang$html$Html$text('');
var _user$project$Main$diffTimeElement = function (time) {
	return A2(
		_elm_lang$html$Html$span,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text('Rozdíl oproti plánu je '),
			_1: {
				ctor: '::',
				_0: _user$project$Main$infoTag(
					_user$project$Utils$timeToString(time)),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html$text('.'),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Main$targetTimeElement = function (time) {
	return A2(
		_elm_lang$html$Html$span,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _elm_lang$html$Html$text('Cílový čas bude '),
			_1: {
				ctor: '::',
				_0: _user$project$Main$infoTag(
					_user$project$Utils$timeToString(time)),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html$text('. '),
					_1: {ctor: '[]'}
				}
			}
		});
};
var _user$project$Main$remainingTimeElement = function (time) {
	var _p0 = time;
	if (_p0.ctor === 'Just') {
		return A2(
			_elm_lang$html$Html$span,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text('Do cíle zbývá '),
				_1: {
					ctor: '::',
					_0: _user$project$Main$infoTag(
						_user$project$Utils$timeToString(_p0._0)),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html$text('. '),
						_1: {ctor: '[]'}
					}
				}
			});
	} else {
		return _user$project$Main$nothing;
	}
};
var _user$project$Main$nextSectionElement = function (nextSection) {
	var _p1 = nextSection;
	if (_p1.ctor === 'Just') {
		return A2(
			_elm_lang$html$Html$span,
			{ctor: '[]'},
			{
				ctor: '::',
				_0: _elm_lang$html$Html$text('Na další úsek se chystá '),
				_1: {
					ctor: '::',
					_0: _user$project$Main$infoTag(_p1._0.runner),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html$text('. '),
						_1: {ctor: '[]'}
					}
				}
			});
	} else {
		return _user$project$Main$nothing;
	}
};
var _user$project$Main$currentSectionElement = F2(
	function (currentSection, currentSectionTime) {
		var timeToFinish = A2(
			_elm_lang$core$Maybe$withDefault,
			0,
			A2(
				_elm_lang$core$Maybe$andThen,
				function (s) {
					return s.timeToFinish;
				},
				currentSection));
		var _p2 = currentSection;
		if (_p2.ctor === 'Just') {
			var _p3 = _p2._0;
			return A2(
				_elm_lang$html$Html$span,
				{ctor: '[]'},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text('Právě běží '),
					_1: {
						ctor: '::',
						_0: _user$project$Main$infoTag(_p3.runner),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html$text(' úsek '),
							_1: {
								ctor: '::',
								_0: _user$project$Main$infoTag(_p3.name),
								_1: {
									ctor: '::',
									_0: _elm_lang$html$Html$text(' a bude v cíli za '),
									_1: {
										ctor: '::',
										_0: _user$project$Main$infoTag(
											_user$project$Utils$timeToString(
												A2(_elm_lang$core$Maybe$withDefault, 0, currentSectionTime))),
										_1: {
											ctor: '::',
											_0: _elm_lang$html$Html$text('. '),
											_1: {ctor: '[]'}
										}
									}
								}
							}
						}
					}
				});
		} else {
			return _user$project$Main$nothing;
		}
	});
var _user$project$Main$iconLinkStyle = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: 'color', _1: 'STEELBLUE'},
	_1: {ctor: '[]'}
};
var _user$project$Main$commonTableCellStyles = {
	ctor: '::',
	_0: {ctor: '_Tuple2', _0: 'vertical-align', _1: 'middle'},
	_1: {ctor: '[]'}
};
var _user$project$Main$higlightedTableCellStyles = A2(
	_elm_lang$core$Basics_ops['++'],
	_user$project$Main$commonTableCellStyles,
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'font-weight', _1: 'bold'},
		_1: {ctor: '[]'}
	});
var _user$project$Main$finishedTableCellStyles = A2(
	_elm_lang$core$Basics_ops['++'],
	_user$project$Main$commonTableCellStyles,
	{
		ctor: '::',
		_0: {ctor: '_Tuple2', _0: 'color', _1: 'Silver'},
		_1: {ctor: '[]'}
	});
var _user$project$Main$timeTableCell = F4(
	function (time, section, sectionFormItem, sectionEditForm) {
		var inputColor = function () {
			var _p4 = sectionEditForm;
			if (_p4.ctor === 'Just') {
				return _p4._0.isRealTimeValid ? _surprisetalk$elm_bulma$Bulma_Modifiers$Default : _surprisetalk$elm_bulma$Bulma_Modifiers$Danger;
			} else {
				return _surprisetalk$elm_bulma$Bulma_Modifiers$Default;
			}
		}();
		var displayValue = function () {
			var _p5 = sectionEditForm;
			if (_p5.ctor === 'Just') {
				var _p7 = _p5._0;
				var _p6 = sectionFormItem;
				switch (_p6.ctor) {
					case 'RealTime':
						return _p7.realTime;
					case 'PlannedTime':
						return _p7.plannedTime;
					default:
						return _user$project$Utils$maybeTimeToString(time);
				}
			} else {
				return _user$project$Utils$maybeTimeToString(time);
			}
		}();
		var inputAttributes = {
			ctor: '::',
			_0: _elm_lang$html$Html_Attributes$value(displayValue),
			_1: {
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$placeholder('mmss'),
				_1: {
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						{
							ctor: '::',
							_0: {ctor: '_Tuple2', _0: 'text-align', _1: 'right'},
							_1: {
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'width', _1: '78px'},
								_1: {ctor: '[]'}
							}
						}),
					_1: {
						ctor: '::',
						_0: _elm_lang$html$Html_Events$onFocus(
							_user$project$Model$StartSectionFormEdit(section)),
						_1: {
							ctor: '::',
							_0: _elm_lang$html$Html_Events$onInput(
								_user$project$Model$UpdateSectionFormItem(sectionFormItem)),
							_1: {ctor: '[]'}
						}
					}
				}
			}
		};
		var isEditable = function () {
			var _p8 = sectionFormItem;
			if (_p8.ctor === 'Readonly') {
				return false;
			} else {
				return true;
			}
		}();
		var _p9 = isEditable;
		if (_p9 === true) {
			return A2(
				_surprisetalk$elm_bulma$Bulma_Elements$tableCell,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(_user$project$Main$commonTableCellStyles),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: A4(
						_surprisetalk$elm_bulma$Bulma_Form$controlInput,
						_elm_lang$core$Native_Utils.update(
							_surprisetalk$elm_bulma$Bulma_Form$controlInputModifiers,
							{size: _surprisetalk$elm_bulma$Bulma_Modifiers$Standard, color: inputColor}),
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(
								{
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'text-align', _1: 'right'},
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						},
						inputAttributes,
						{ctor: '[]'}),
					_1: {ctor: '[]'}
				});
		} else {
			return A2(
				_surprisetalk$elm_bulma$Bulma_Elements$tableCell,
				{
					ctor: '::',
					_0: _elm_lang$html$Html_Attributes$style(
						A2(
							_elm_lang$core$Basics_ops['++'],
							_user$project$Main$commonTableCellStyles,
							{
								ctor: '::',
								_0: {ctor: '_Tuple2', _0: 'text-align', _1: 'right'},
								_1: {ctor: '[]'}
							})),
					_1: {ctor: '[]'}
				},
				{
					ctor: '::',
					_0: _elm_lang$html$Html$text(displayValue),
					_1: {ctor: '[]'}
				});
		}
	});
var _user$project$Main$sectionTableRow = F4(
	function (currentSection, sectionEditForm, raceStartTime, section) {
		var isBigChangeOver = _elm_lang$core$Native_Utils.eq(
			A2(_elm_lang$core$Basics$rem, section.order, 6),
			0) && (!_elm_lang$core$Native_Utils.eq(section.order, 36));
		var startTime = _user$project$Utils$sectionStartTimeToString(section.timeToStart);
		var isFinished = function () {
			var _p10 = currentSection;
			if (_p10.ctor === 'Nothing') {
				return false;
			} else {
				return _elm_lang$core$Native_Utils.cmp(_p10._0.order, section.order) > 0;
			}
		}();
		var isEdited = _elm_lang$core$Native_Utils.eq(sectionEditForm.sectionId, section.id);
		var editForm = isEdited ? _elm_lang$core$Maybe$Just(sectionEditForm) : _elm_lang$core$Maybe$Nothing;
		var isCurrentRow = function () {
			var _p11 = currentSection;
			if (_p11.ctor === 'Nothing') {
				return false;
			} else {
				return _elm_lang$core$Native_Utils.eq(section.id, _p11._0.id);
			}
		}();
		var cellStyle = isCurrentRow ? _user$project$Main$higlightedTableCellStyles : (isFinished ? _user$project$Main$finishedTableCellStyles : _user$project$Main$commonTableCellStyles);
		return A3(
			_surprisetalk$elm_bulma$Bulma_Elements$tableRow,
			isCurrentRow,
			{
				ctor: '::',
				_0: _elm_lang$html$Html_Attributes$style(
					isBigChangeOver ? {
						ctor: '::',
						_0: {ctor: '_Tuple2', _0: 'border-bottom', _1: 'solid 4px Gold  '},
						_1: {ctor: '[]'}
					} : {ctor: '[]'}),
				_1: {ctor: '[]'}
			},
			{
				ctor: '::',
				_0: A2(
					_surprisetalk$elm_bulma$Bulma_Elements$tableCell,
					{
						ctor: '::',
						_0: _elm_lang$html$Html_Attributes$style(cellStyle),
						_1: {ctor: '[]'}
					},
					{
						ctor: '::',
						_0: _elm_lang$html$Html$text(section.name),
						_1: {ctor: '[]'}
					}),
				_1: {
					ctor: '::',
					_0: A2(
						_surprisetalk$elm_bulma$Bulma_Elements$tableCell,
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(cellStyle),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$a,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$href(section.runnerRoute),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$target('_blank'),
										_1: {
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$style(_user$project$Main$iconLinkStyle),
											_1: {ctor: '[]'}
										}
									}
								},
								{
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$i,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$class('fa fa-map-marker-alt'),
											_1: {ctor: '[]'}
										},
										{ctor: '[]'}),
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_surprisetalk$elm_bulma$Bulma_Elements$tableCell,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$style(cellStyle),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A2(
									_elm_lang$html$Html$a,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$href(section.carRoute),
										_1: {
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$target('_blank'),
											_1: {
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$style(_user$project$Main$iconLinkStyle),
												_1: {ctor: '[]'}
											}
										}
									},
									{
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html$i,
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$class('fa fa-car'),
												_1: {ctor: '[]'}
											},
											{ctor: '[]'}),
										_1: {ctor: '[]'}
									}),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_surprisetalk$elm_bulma$Bulma_Elements$tableCell,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$style(cellStyle),
									_1: {ctor: '[]'}
								},
								{
									ctor: '::',
									_0: A2(
										_elm_lang$html$Html$a,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$href(section.proposition),
											_1: {
												ctor: '::',
												_0: _elm_lang$html$Html_Attributes$target('_blank'),
												_1: {
													ctor: '::',
													_0: _elm_lang$html$Html_Attributes$style(_user$project$Main$iconLinkStyle),
													_1: {ctor: '[]'}
												}
											}
										},
										{
											ctor: '::',
											_0: A2(
												_elm_lang$html$Html$i,
												{
													ctor: '::',
													_0: _elm_lang$html$Html_Attributes$class('fa fa-info-circle'),
													_1: {ctor: '[]'}
												},
												{ctor: '[]'}),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A2(
									_surprisetalk$elm_bulma$Bulma_Elements$tableCell,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$style(cellStyle),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: _elm_lang$html$Html$text(section.runner),
										_1: {ctor: '[]'}
									}),
								_1: {
									ctor: '::',
									_0: A2(
										_surprisetalk$elm_bulma$Bulma_Elements$tableCell,
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$style(
												A2(
													_elm_lang$core$Basics_ops['++'],
													cellStyle,
													{
														ctor: '::',
														_0: {ctor: '_Tuple2', _0: 'text-align', _1: 'right'},
														_1: {ctor: '[]'}
													})),
											_1: {ctor: '[]'}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text(startTime),
											_1: {ctor: '[]'}
										}),
									_1: {
										ctor: '::',
										_0: A4(
											_user$project$Main$timeTableCell,
											_elm_lang$core$Maybe$Just(section.plannedTime),
											section,
											_user$project$Model$Readonly,
											editForm),
										_1: {
											ctor: '::',
											_0: A4(_user$project$Main$timeTableCell, section.realTime, section, _user$project$Model$RealTime, editForm),
											_1: {
												ctor: '::',
												_0: A2(
													_surprisetalk$elm_bulma$Bulma_Elements$tableCell,
													{
														ctor: '::',
														_0: _elm_lang$html$Html_Attributes$style(_user$project$Main$commonTableCellStyles),
														_1: {ctor: '[]'}
													},
													{
														ctor: '::',
														_0: isCurrentRow ? A3(
															_surprisetalk$elm_bulma$Bulma_Elements$button,
															_elm_lang$core$Native_Utils.update(
																_surprisetalk$elm_bulma$Bulma_Elements$buttonModifiers,
																{color: _surprisetalk$elm_bulma$Bulma_Modifiers$Warning, size: _surprisetalk$elm_bulma$Bulma_Modifiers$Small}),
															{
																ctor: '::',
																_0: _elm_lang$html$Html_Events$onClick(
																	_user$project$Model$Changeover(section)),
																_1: {ctor: '[]'}
															},
															{
																ctor: '::',
																_0: A2(
																	_elm_lang$html$Html$i,
																	{
																		ctor: '::',
																		_0: _elm_lang$html$Html_Attributes$class('fa fa-check'),
																		_1: {ctor: '[]'}
																	},
																	{ctor: '[]'}),
																_1: {
																	ctor: '::',
																	_0: _elm_lang$html$Html$text(' Předávka'),
																	_1: {ctor: '[]'}
																}
															}) : _user$project$Main$nothing,
														_1: {
															ctor: '::',
															_0: (isEdited && (!isCurrentRow)) ? A3(
																_surprisetalk$elm_bulma$Bulma_Elements$button,
																_elm_lang$core$Native_Utils.update(
																	_surprisetalk$elm_bulma$Bulma_Elements$buttonModifiers,
																	{color: _surprisetalk$elm_bulma$Bulma_Modifiers$Primary, size: _surprisetalk$elm_bulma$Bulma_Modifiers$Small}),
																{
																	ctor: '::',
																	_0: _elm_lang$html$Html_Events$onClick(
																		_user$project$Model$ValidateAndSaveEditForm(section)),
																	_1: {ctor: '[]'}
																},
																{
																	ctor: '::',
																	_0: A2(
																		_elm_lang$html$Html$i,
																		{
																			ctor: '::',
																			_0: _elm_lang$html$Html_Attributes$class('fa fa-check'),
																			_1: {ctor: '[]'}
																		},
																		{ctor: '[]'}),
																	_1: {
																		ctor: '::',
																		_0: _elm_lang$html$Html$text(' Uložit'),
																		_1: {ctor: '[]'}
																	}
																}) : _user$project$Main$nothing,
															_1: {ctor: '[]'}
														}
													}),
												_1: {ctor: '[]'}
											}
										}
									}
								}
							}
						}
					}
				}
			});
	});
var _user$project$Main$widths = function (width) {
	return {mobile: width, tablet: width, desktop: width, widescreen: width, fullHD: width};
};
var _user$project$Main$buttonsColumnWidths = {
	mobile: _elm_lang$core$Maybe$Just(_surprisetalk$elm_bulma$Bulma_Modifiers$Width9),
	tablet: _elm_lang$core$Maybe$Just(_surprisetalk$elm_bulma$Bulma_Modifiers$Width5),
	desktop: _elm_lang$core$Maybe$Nothing,
	widescreen: _elm_lang$core$Maybe$Nothing,
	fullHD: _elm_lang$core$Maybe$Nothing
};
var _user$project$Main$menu = function (startButtonVisible) {
	return A3(
		_surprisetalk$elm_bulma$Bulma_Components$navbar,
		_surprisetalk$elm_bulma$Bulma_Components$navbarModifiers,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: A3(
				_surprisetalk$elm_bulma$Bulma_Components$navbarBrand,
				{ctor: '[]'},
				A3(
					_surprisetalk$elm_bulma$Bulma_Components$navbarBurger,
					false,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A3(
							_surprisetalk$elm_bulma$Bulma_Components$navbarItem,
							true,
							{ctor: '[]'},
							{ctor: '[]'}),
						_1: {ctor: '[]'}
					}),
				{
					ctor: '::',
					_0: A3(
						_surprisetalk$elm_bulma$Bulma_Components$navbarItem,
						false,
						{ctor: '[]'},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$img,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$src('./logo_gomango.jpg'),
									_1: {ctor: '[]'}
								},
								{ctor: '[]'}),
							_1: {ctor: '[]'}
						}),
					_1: {ctor: '[]'}
				}),
			_1: {
				ctor: '::',
				_0: A3(
					_surprisetalk$elm_bulma$Bulma_Components$navbarMenu,
					true,
					{ctor: '[]'},
					{
						ctor: '::',
						_0: A2(
							_surprisetalk$elm_bulma$Bulma_Components$navbarEnd,
							{ctor: '[]'},
							{
								ctor: '::',
								_0: startButtonVisible ? A3(
									_surprisetalk$elm_bulma$Bulma_Components$navbarItem,
									true,
									{ctor: '[]'},
									{
										ctor: '::',
										_0: A3(
											_surprisetalk$elm_bulma$Bulma_Elements$button,
											_elm_lang$core$Native_Utils.update(
												_surprisetalk$elm_bulma$Bulma_Elements$buttonModifiers,
												{color: _surprisetalk$elm_bulma$Bulma_Modifiers$Success}),
											{
												ctor: '::',
												_0: _elm_lang$html$Html_Events$onClick(_user$project$Model$Start),
												_1: {ctor: '[]'}
											},
											{
												ctor: '::',
												_0: A2(
													_elm_lang$html$Html$i,
													{
														ctor: '::',
														_0: _elm_lang$html$Html_Attributes$class('fa fa-play-circle'),
														_1: {ctor: '[]'}
													},
													{ctor: '[]'}),
												_1: {
													ctor: '::',
													_0: _elm_lang$html$Html$text(' Start!'),
													_1: {ctor: '[]'}
												}
											}),
										_1: {ctor: '[]'}
									}) : _user$project$Main$nothing,
								_1: {
									ctor: '::',
									_0: A3(
										_surprisetalk$elm_bulma$Bulma_Components$navbarItem,
										true,
										{ctor: '[]'},
										{
											ctor: '::',
											_0: A3(
												_surprisetalk$elm_bulma$Bulma_Elements$button,
												_elm_lang$core$Native_Utils.update(
													_surprisetalk$elm_bulma$Bulma_Elements$buttonModifiers,
													{color: _surprisetalk$elm_bulma$Bulma_Modifiers$Light}),
												{
													ctor: '::',
													_0: _elm_lang$html$Html_Events$onClick(_user$project$Model$Reload),
													_1: {ctor: '[]'}
												},
												{
													ctor: '::',
													_0: A2(
														_elm_lang$html$Html$i,
														{
															ctor: '::',
															_0: _elm_lang$html$Html_Attributes$class('fa fa-sync'),
															_1: {ctor: '[]'}
														},
														{ctor: '[]'}),
													_1: {
														ctor: '::',
														_0: _elm_lang$html$Html$text(' Aktualizovat'),
														_1: {ctor: '[]'}
													}
												}),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}
							}),
						_1: {ctor: '[]'}
					}),
				_1: {ctor: '[]'}
			}
		});
};
var _user$project$Main$view = function (model) {
	var startButtonVisible = function () {
		var _p12 = model.startTime;
		if (_p12.ctor === 'Nothing') {
			return true;
		} else {
			return false;
		}
	}();
	return A2(
		_elm_lang$html$Html$div,
		{ctor: '[]'},
		{
			ctor: '::',
			_0: _surprisetalk$elm_bulma$Bulma_CDN$stylesheet,
			_1: {
				ctor: '::',
				_0: _user$project$Main$menu(startButtonVisible),
				_1: {
					ctor: '::',
					_0: A3(
						_surprisetalk$elm_bulma$Bulma_Layout$hero,
						_elm_lang$core$Native_Utils.update(
							_surprisetalk$elm_bulma$Bulma_Layout$heroModifiers,
							{color: _surprisetalk$elm_bulma$Bulma_Modifiers$Warning}),
						{
							ctor: '::',
							_0: _elm_lang$html$Html_Attributes$style(
								{
									ctor: '::',
									_0: {ctor: '_Tuple2', _0: 'padding', _1: '12px'},
									_1: {ctor: '[]'}
								}),
							_1: {ctor: '[]'}
						},
						{
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$span,
								{ctor: '[]'},
								{ctor: '[]'}),
							_1: {
								ctor: '::',
								_0: A2(
									_surprisetalk$elm_bulma$Bulma_Layout$heroHead,
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$style(
											{ctor: '[]'}),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: A2(
											_elm_lang$html$Html$div,
											{ctor: '[]'},
											{
												ctor: '::',
												_0: A2(_user$project$Main$currentSectionElement, model.currentSection, model.currentSectionTime),
												_1: {
													ctor: '::',
													_0: _user$project$Main$nextSectionElement(model.nextSection),
													_1: {ctor: '[]'}
												}
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_elm_lang$html$Html$div,
												{ctor: '[]'},
												{
													ctor: '::',
													_0: _user$project$Main$remainingTimeElement(model.remainingTime),
													_1: {
														ctor: '::',
														_0: _user$project$Main$targetTimeElement(model.targetTime),
														_1: {
															ctor: '::',
															_0: _user$project$Main$diffTimeElement(model.diffTime),
															_1: {ctor: '[]'}
														}
													}
												}),
											_1: {ctor: '[]'}
										}
									}),
								_1: {ctor: '[]'}
							}
						}),
					_1: {
						ctor: '::',
						_0: A2(
							_elm_lang$html$Html$div,
							{
								ctor: '::',
								_0: _elm_lang$html$Html_Attributes$class('container'),
								_1: {ctor: '[]'}
							},
							{
								ctor: '::',
								_0: A3(
									_surprisetalk$elm_bulma$Bulma_Elements$table,
									_elm_lang$core$Native_Utils.update(
										_surprisetalk$elm_bulma$Bulma_Elements$tableModifiers,
										{narrow: true, hoverable: true, fullWidth: true}),
									{
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$style(
											{
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'margin', _1: 'auto'},
												_1: {
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'margin-top', _1: '24px'},
													_1: {ctor: '[]'}
												}
											}),
										_1: {ctor: '[]'}
									},
									{
										ctor: '::',
										_0: A2(
											_surprisetalk$elm_bulma$Bulma_Elements$tableHead,
											{ctor: '[]'},
											{
												ctor: '::',
												_0: A3(
													_surprisetalk$elm_bulma$Bulma_Elements$tableRow,
													false,
													{ctor: '[]'},
													{
														ctor: '::',
														_0: A2(
															_surprisetalk$elm_bulma$Bulma_Elements$tableCellHead,
															{ctor: '[]'},
															{
																ctor: '::',
																_0: _elm_lang$html$Html$text('Úsek'),
																_1: {ctor: '[]'}
															}),
														_1: {
															ctor: '::',
															_0: A2(
																_surprisetalk$elm_bulma$Bulma_Elements$tableCellHead,
																{ctor: '[]'},
																{
																	ctor: '::',
																	_0: _elm_lang$html$Html$text(''),
																	_1: {ctor: '[]'}
																}),
															_1: {
																ctor: '::',
																_0: A2(
																	_surprisetalk$elm_bulma$Bulma_Elements$tableCellHead,
																	{ctor: '[]'},
																	{
																		ctor: '::',
																		_0: _elm_lang$html$Html$text(''),
																		_1: {ctor: '[]'}
																	}),
																_1: {
																	ctor: '::',
																	_0: A2(
																		_surprisetalk$elm_bulma$Bulma_Elements$tableCellHead,
																		{ctor: '[]'},
																		{
																			ctor: '::',
																			_0: _elm_lang$html$Html$text(''),
																			_1: {ctor: '[]'}
																		}),
																	_1: {
																		ctor: '::',
																		_0: A2(
																			_surprisetalk$elm_bulma$Bulma_Elements$tableCellHead,
																			{ctor: '[]'},
																			{
																				ctor: '::',
																				_0: _elm_lang$html$Html$text('Borec'),
																				_1: {ctor: '[]'}
																			}),
																		_1: {
																			ctor: '::',
																			_0: A2(
																				_surprisetalk$elm_bulma$Bulma_Elements$tableCellHead,
																				{
																					ctor: '::',
																					_0: _elm_lang$html$Html_Attributes$style(
																						{
																							ctor: '::',
																							_0: {ctor: '_Tuple2', _0: 'text-align', _1: 'right'},
																							_1: {
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'min-width', _1: '84px'},
																								_1: {ctor: '[]'}
																							}
																						}),
																					_1: {ctor: '[]'}
																				},
																				{
																					ctor: '::',
																					_0: _elm_lang$html$Html$text('Start'),
																					_1: {ctor: '[]'}
																				}),
																			_1: {
																				ctor: '::',
																				_0: A2(
																					_surprisetalk$elm_bulma$Bulma_Elements$tableCellHead,
																					{
																						ctor: '::',
																						_0: _elm_lang$html$Html_Attributes$style(
																							{
																								ctor: '::',
																								_0: {ctor: '_Tuple2', _0: 'text-align', _1: 'right'},
																								_1: {
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'min-width', _1: '84px'},
																									_1: {ctor: '[]'}
																								}
																							}),
																						_1: {ctor: '[]'}
																					},
																					{
																						ctor: '::',
																						_0: _elm_lang$html$Html$text('Plán'),
																						_1: {ctor: '[]'}
																					}),
																				_1: {
																					ctor: '::',
																					_0: A2(
																						_surprisetalk$elm_bulma$Bulma_Elements$tableCellHead,
																						{
																							ctor: '::',
																							_0: _elm_lang$html$Html_Attributes$style(
																								{
																									ctor: '::',
																									_0: {ctor: '_Tuple2', _0: 'text-align', _1: 'right'},
																									_1: {
																										ctor: '::',
																										_0: {ctor: '_Tuple2', _0: 'min-width', _1: '84px'},
																										_1: {ctor: '[]'}
																									}
																								}),
																							_1: {ctor: '[]'}
																						},
																						{
																							ctor: '::',
																							_0: _elm_lang$html$Html$text('Skutečný čas'),
																							_1: {ctor: '[]'}
																						}),
																					_1: {
																						ctor: '::',
																						_0: A2(
																							_surprisetalk$elm_bulma$Bulma_Elements$tableCellHead,
																							{ctor: '[]'},
																							{
																								ctor: '::',
																								_0: _elm_lang$html$Html$text(''),
																								_1: {ctor: '[]'}
																							}),
																						_1: {ctor: '[]'}
																					}
																				}
																			}
																		}
																	}
																}
															}
														}
													}),
												_1: {ctor: '[]'}
											}),
										_1: {
											ctor: '::',
											_0: A2(
												_surprisetalk$elm_bulma$Bulma_Elements$tableBody,
												{ctor: '[]'},
												A2(
													_elm_lang$core$List$map,
													A3(_user$project$Main$sectionTableRow, model.currentSection, model.sectionEditForm, model.startTime),
													model.sections)),
											_1: {ctor: '[]'}
										}
									}),
								_1: {ctor: '[]'}
							}),
						_1: {
							ctor: '::',
							_0: A2(
								_elm_lang$html$Html$div,
								{
									ctor: '::',
									_0: _elm_lang$html$Html_Attributes$class('container'),
									_1: {
										ctor: '::',
										_0: _elm_lang$html$Html_Attributes$style(
											{
												ctor: '::',
												_0: {ctor: '_Tuple2', _0: 'text-align', _1: 'left'},
												_1: {ctor: '[]'}
											}),
										_1: {ctor: '[]'}
									}
								},
								{
									ctor: '::',
									_0: A3(
										_surprisetalk$elm_bulma$Bulma_Elements$button,
										_elm_lang$core$Native_Utils.update(
											_surprisetalk$elm_bulma$Bulma_Elements$buttonModifiers,
											{color: _surprisetalk$elm_bulma$Bulma_Modifiers$Default}),
										{
											ctor: '::',
											_0: _elm_lang$html$Html_Attributes$style(
												{
													ctor: '::',
													_0: {ctor: '_Tuple2', _0: 'margin', _1: '8px'},
													_1: {ctor: '[]'}
												}),
											_1: {
												ctor: '::',
												_0: _elm_lang$html$Html_Events$onClick(_user$project$Model$OpenResetDialog),
												_1: {ctor: '[]'}
											}
										},
										{
											ctor: '::',
											_0: _elm_lang$html$Html$text('Reset'),
											_1: {ctor: '[]'}
										}),
									_1: {ctor: '[]'}
								}),
							_1: {
								ctor: '::',
								_0: A4(_pablen$toasty$Toasty$view, _pablen$toasty$Toasty_Defaults$config, _pablen$toasty$Toasty_Defaults$view, _user$project$Model$ToastyMsg, model.toasties),
								_1: {
									ctor: '::',
									_0: _user$project$Main$resetModal(model.isResetModalOpen),
									_1: {ctor: '[]'}
								}
							}
						}
					}
				}
			}
		});
};
var _user$project$Main$main = _elm_lang$html$Html$program(
	{view: _user$project$Main$view, update: _user$project$Update$update, init: _user$project$Update$init, subscriptions: _user$project$Update$subscriptions})();

var Elm = {};
Elm['Main'] = Elm['Main'] || {};
if (typeof _user$project$Main$main !== 'undefined') {
    _user$project$Main$main(Elm['Main'], 'Main', undefined);
}

if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);

