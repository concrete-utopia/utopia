export const forOwn = (obj, callback, thisArg) => {
  for (let key in obj) {
    if (Object.prototype.hasOwnProperty.call(obj, key)) {
      if (callback && callback.call(thisArg, obj[key], key, obj) === false) {
        return;
      }
    }
  }

  return obj;
};

export const alias = (fn, instance) => (...args) => instance[fn].apply(instance, args);

export const deliverTopic = (topic, data, instance) => {
  const topics = instance._pubsub_topics;
  const subscribers = topics[topic] ? [...topics[topic]] : [];

  for (let i = 0, len = subscribers.length; i < len; i += 1) {
    const token = subscribers[i].token;
    const currentSubscriber = subscribers[i];

    if (!instance._options.immediateExceptions) {
      try {
        currentSubscriber.callback(data, { name: topic, token: token });
      } catch (exception) {
        setTimeout(() => {
          throw exception;
        }, 0);
      }
    } else {
      currentSubscriber.callback(data, { name: topic, token: token });
    }

    // Unsubscribe from event based on tokenized reference,
    // if subscriber's property once is set to true.
    if (currentSubscriber.once === true) {
      instance.unsubscribe(token);
    }
  }
};

export const publishData = (topic, ...data) => data.length <= 1 ? data[0] : [...data];

export const publish = (instance, topic, data, sync) => {
  const topics = instance._pubsub_topics;

  if (!topics[topic]) {
    return false;
  }

  sync ? deliverTopic(topic, data, instance) : setTimeout(() => {
    deliverTopic(topic, data, instance);
  }, 0);

  return true;
};
