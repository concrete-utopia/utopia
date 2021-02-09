import { forOwn, publish, publishData, alias } from './utils';

class PubSub {
  /**
   * Creates a PubSub instance.
   * @constructor PubSub
   *
   * @param {object} [options] User options
   * @param {boolean} [options.immediateExceptions=false] Forces exceptions to be thrown immediately instead of delayed exceptions
   */
  constructor(options) {
    const defaults = {
      immediateExceptions: false
    };

    this._pubsub_topics = {}; // Storage for topics that can be broadcast or listened to.
    this._pubsub_uid = -1; // A topic identifier.
    this._options = { ...defaults, ...options };
  }

  /**
   * Subscribe to events of interest with a specific topic name and a
   * callback function, to be executed when the topic/event is observed.
   *
   * @memberof PubSub
   * @this {PubSub}
   * @param {string} topic The topic's name
   * @param {function} callback Callback function to execute on event, taking two arguments:
   *        - {*} data The data passed when publishing an event
   *        - {object} The topic's info (name & token)
   * @param {boolean} [once=false] Checks if event will be triggered only one time
   * @return {number} The topic's token
   * @example
   *
   * const pubsub = new PubSub();
   *
   * const onUserAdd = pubsub.subscribe('user_add', (data, topic) => {
   *   console.log('User added');
   *   console.log('user data:', data);
   * });
   */
  subscribe(topic, callback, once) {
    const topics = this._pubsub_topics;
    const token = this._pubsub_uid += 1;
    const obj = {};

    if (typeof callback !== 'function') {
      throw new TypeError('When subscribing for an event, a callback function must be defined.');
    }

    if (!topics[topic]) {
      topics[topic] = [];
    }

    obj.token = token;
    obj.callback = callback;
    obj.once = !!once;

    topics[topic].push(obj);

    return token;
  }

  /**
   * Subscribe to events of interest setting a flag
   * indicating the event will be published only one time.
   *
   * @memberof PubSub
   * @this {PubSub}
   * @param {string} topic The topic's name
   * @param {function} callback Callback function to execute on event, taking two arguments:
   *        - {*} data The data passed when publishing an event
   *        - {object} The topic's info (name & token)
   * @return {number} The topic's token
   * @example
   *
   * const onUserAdd = pubsub.subscribeOnce('user_add', (data, topic) => {
   *   console.log('User added');
   *   console.log('user data:', data);
   * });
   */
  subscribeOnce(topic, callback) {
    return this.subscribe(topic, callback, true);
  }

  /**
   * Publishes a topic asynchronously, passing the data to its subscribers.
   * Asynchronous publication helps in that the originator of the topics will
   * not be blocked while consumers process them.
   * For synchronous topic publication check `publishSync`.
   *
   * @memberof PubSub
   * @this {PubSub}
   * @param {string} topic The topic's name
   * @param {...*} [data] The data to be passed to its subscribers
   * @return {boolean} Returns `true` if topic exists and event is published; otheriwse `false`
   * @example
   *
   * pubsub.publish('user_add', {
   *   firstName: 'John',
   *   lastName: 'Doe',
   *   email: 'johndoe@gmail.com'
   * });
   */
  publish(topic, ...data) {
    return publish(this, topic, publishData(topic, ...data), false);
  }

  /**
   * Publishes a topic synchronously, passing the data to its subscribers.
   *
   * @memberof PubSub
   * @this {PubSub}
   * @param {string} topic The topic's name
   * @param {...*} [data] The data to be passed to its subscribers
   * @return {boolean} Returns `true` if topic exists and event is published; otheriwse `false`
   * @example
   *
   * pubsub.publishSync('user_add', {
   *   firstName: 'John',
   *   lastName: 'Doe',
   *   email: 'johndoe@gmail.com'
   * });
   */
  publishSync(topic, ...data) {
    return publish(this, topic, publishData(topic, ...data), true);
  }

  /**
   * Unsubscribes from a specific topic, based on the topic name,
   * or based on a tokenized reference to the subscription.
   *
   * @memberof PubSub
   * @this {PubSub}
   * @param {string|number} topic Topic's name or subscription reference
   * @return {boolean|string} Returns `false` if `topic` does not match a subscribed event; otherwise the topic's name
   * @example
   *
   * // Unsubscribe using the topic's name.
   * pubsub.unsubscribe('user_add');
   *
   * // Unsubscribe using a tokenized reference to the subscription.
   * pubsub.unsubscribe(onUserAdd);
   */
  unsubscribe(topic) {
    const topics = this._pubsub_topics;
    let tf = false;

    for (const prop in topics) {
      if (Object.prototype.hasOwnProperty.call(topics, prop)) {
        if (topics[prop]) {
          let len = topics[prop].length;

          while (len) {
            len -= 1;

            // `topic` is a tokenized reference to the subscription.
            if (topics[prop][len].token === topic) {
              topics[prop].splice(len, 1);
              if (topics[prop].length === 0) {
                delete topics[prop];
              }
              return topic;
            }

            // `topic` is the event name.
            if (prop === topic) {
              topics[prop].splice(len, 1);
              if (topics[prop].length === 0) {
                delete topics[prop];
              }
              tf = true;
            }
          }

          if (tf === true) {
            return topic;
          }
        }
      }
    }

    return false;
  }

  /**
   * Clears all subscriptions whatsoever.
   *
   * @memberof PubSub
   * @this {PubSub}
   * @return {PubSub} The PubSub instance.
   * @example
   *
   * const pubsub = new PubSub();
   * pubsub.subscribe('message1', () => {});
   * pubsub.subscribe('message2', () => {});
   * pubsub.subscribe('message3', () => {});
   * pubsub.unsubscribeAll();
   * pubsub.hasSubscribers(); // -> false
   */
  unsubscribeAll() {
    this._pubsub_topics = {};
    return this;
  }

  /**
   * Checks if there are subscribers for a specific topic.
   * If `topic` is not provided, checks if there is at least one subscriber.
   *
   * @memberof PubSub
   * @this {PubSub}
   * @param {string} [topic] The topic's name to check
   * @return {boolean} Returns `true` there are subscribers; otherwise `false`
   * @example
   *
   * const pubsub = new PubSub();
   * pubsub.on('message', data => console.log(data));
   *
   * pubsub.hasSubscribers('message');
   * // -> true
   */
  hasSubscribers(topic) {
    const topics = this._pubsub_topics;
    let hasSubscribers = false;

    // If no arguments passed
    if (topic == null) {
      forOwn(topics, (value, key) => {
        if (key) {
          hasSubscribers = true;
          return false;
        }
      });

      return hasSubscribers;
    }

    // If a topic's name is passed as argument
    return Object.prototype.hasOwnProperty.call(topics, topic);
  }

  /**
   * Gets all the subscribers as a set of key value pairs that
   * represent the topic's name and the event listener(s) bound.
   *
   * @NOTE Mutating the result of this method does not affect the real subscribers. This is for reference only.
   *
   * @memberof PubSub
   * @this {PubSub}
   * @return {object} A readonly object with all subscribers.
   * @example
   *
   * const pubsub = new PubSub();
   *
   * pubsub.subscribe('message', listener);
   * pubsub.subscribe('message', listener);
   * pubsub.subscribe('another_message', listener);
   *
   * pubsub.subscribers();
   * // -> Object { message: Array[2], another_message: Array[1] }
   */
  subscribers() {
    const res = {};

    forOwn(this._pubsub_topics, (topicValue, topicKey) => {
      res[topicKey] = [...topicValue];
    });

    return res;
  }

  /**
   * Gets subscribers for a specific topic.
   *
   * @NOTE Mutating the result of this method does not affect the real subscribers. This is for reference only.
   *
   * @memberof PubSub
   * @this {PubSub}
   * @param {string} topic The topic's name to check for subscribers
   * @return {array} A copy array of all subscribers for a topic if exist; otherwise an empty array
   * @example
   *
   * const pubsub = new PubSub();
   *
   * pubsub.subscribe('message', listener1);
   * pubsub.subscribeOnce('message', listener2);
   * pubsub.subscribe('another_message', listener1);
   *
   * pubsub.subscribersByTopic('message');
   * // -> Array [{token: 0, once: false, callback: listener1()}, {token: 1, once: true, callback: listener2()}]
   *
   * pubsub.subscribersByTopic('another_message');
   * // -> Array [{token: 2, once: false, callback: listener1()}]
   *
   * pubsub.subscribersByTopic('some_message_not_existing');
   * // -> Array []
   */
  subscribersByTopic(topic) {
    return this._pubsub_topics[topic] ? [...this._pubsub_topics[topic]] : [];
  }

  /**
   * Creates aliases for public methods.
   *
   * @memberof PubSub
   * @this {PubSub}
   * @param {object} aliasMap A plain object that maps the public methods to their aliases.
   * @return {PubSub} The PubSub instance.
   * @example
   *
   * const pubsub = new PubSub().alias({
   *   subscribe: 'on',
   *   subscribeOnce: 'once',
   *   publish: 'trigger',
   *   publishSync: 'triggerSync',
   *   unsubscribe: 'off',
   *   hasSubscribers: 'has'
   * });
   */
  alias(aliasMap) {
    forOwn(aliasMap, (value, key) => {
      if (PubSub.prototype[key]) {
        PubSub.prototype[aliasMap[key]] = alias(key, this);
      }
    });

    return this;
  }
}

PubSub.createInstance = options => new PubSub(options);

export default PubSub;
