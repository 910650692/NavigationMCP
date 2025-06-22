package com.sgm.navi.service.logicpaket.message;

import com.sgm.navi.service.adapter.message.MessageCenterAdapter;
import com.sgm.navi.service.adapter.message.MessageCenterAdapterCallBack;
import com.sgm.navi.service.define.message.MessageCenterInfo;
import java.util.Hashtable;

public final class MessageCenterPackage implements MessageCenterAdapterCallBack {

    public static final String MESSAGECENTERKEY = "MessageCenterKey";

    //1.先从package调取adapter
    final private MessageCenterAdapter messageCenterAdapter;
    private final Hashtable<String, MessageCenterCallBack> mCallBacks;

    private MessageCenterPackage() {
        messageCenterAdapter = MessageCenterAdapter.getInstance();
        mCallBacks = new Hashtable<>();
    }

    /**
     * 初始化
     */
    public void initMessageCenter() {
        messageCenterAdapter.initMessageCenter();
        messageCenterAdapter.registerCallBack("MessageCenterPackage", this);
    }

    public static MessageCenterPackage getInstance() {
        return Helper.MESSAGEPACKAGE;
    }

    private static final class Helper {
        private static final MessageCenterPackage MESSAGEPACKAGE = new MessageCenterPackage();
    }

    /**
     * @param key    key
     * @param callback  callback
     */
    public synchronized void registerCallBack(final String key,final MessageCenterCallBack callback) {
        if (callback != null && !mCallBacks.contains(callback)) {
            mCallBacks.put(key,callback);
        }
    }

    /**
     * @param messageCenterInfo    数据
     */
    public void pushMessage(final MessageCenterInfo messageCenterInfo) {
        messageCenterAdapter.pushMessage(messageCenterInfo);
    }

    /**
     * @param msgType    数据
     */
    public void getMessage(final int msgType) {
        messageCenterAdapter.getMessage(msgType);

    }

    /**
     * @param messageCenterInfo    数据
     */
    @Override
    public void onMessageInfoNotifyCallback(final MessageCenterInfo messageCenterInfo) {
        if (null != mCallBacks) {
            for (MessageCenterCallBack observer : mCallBacks.values()) {
                observer.onMessageInfoNotifyCallback(messageCenterInfo);
            }
        }
    }

    @Override
    public void onMessageInfoRemoveCallback() {
        if (null != mCallBacks) {
            for (MessageCenterCallBack observer : mCallBacks.values()) {
                observer.onMessageInfoRemoveCallback();
            }
        }
    }
}
