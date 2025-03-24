package com.fy.navi.service.adapter.message;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.message.MessageCenterInfo;
import java.util.Objects;


public final class MessageCenterAdapter {

    private static final String CLASS_API_PKG = Objects.requireNonNull(MessageCenterAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "MessageCenterAdapterImpl";
    private final IMessageCenterApi mMessageCenterApi;

    private MessageCenterAdapter() {
        mMessageCenterApi = (IMessageCenterApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    /**
     * 初始化
     */
    public void initMessageCenter() {
        mMessageCenterApi.init();
    }

    //设置单例对象
    public static MessageCenterAdapter getInstance() {
        return Helper.ADAPTERHELPER;
    }

    private static final class Helper {
        private static final MessageCenterAdapter ADAPTERHELPER = new MessageCenterAdapter();
    }

    /**
     * @param key key
     * @param callBack 回调
     */
    public void registerCallBack(final String key, final MessageCenterAdapterCallBack callBack) {
        mMessageCenterApi.registerCallBack(key, callBack);
    }

    /**
     * @param messageCenterInfo 数据
     */
    public void pushMessage(final MessageCenterInfo messageCenterInfo) {
        mMessageCenterApi.pushMessage(messageCenterInfo);
    }

    /**
     * @param msgType 类型
     */
    public void getMessage(final int msgType) {
        mMessageCenterApi.getMessage(msgType);
    }

}

