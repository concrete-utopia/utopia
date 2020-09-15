/* eslint new-cap: ["error", { "capIsNew": false }] */
import test from 'ava';
import Window from '../';

test('jsdom config is passed through', t => {
	const userAgent = 'Custom user agent';
	const window = new Window({ userAgent });
	t.is(window.navigator.userAgent, userAgent);
});
