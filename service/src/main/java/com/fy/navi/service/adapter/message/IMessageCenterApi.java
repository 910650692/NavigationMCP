package com.fy.navi.service.adapter.message;

import com.fy.navi.service.define.message.MessageCenterInfo;

public interface IMessageCenterApi {

    /**
     * 初始化
     */
    void init();

    /**
     * @param key key
     * @param callBack 回调
     */
    void registerCallBack(String key, MessageCenterAdapterCallBack callBack);

    /**
     * @param key key
     * @param callBack 回调
     */
    void unRegisterCallback(String key, MessageCenterAdapterCallBack callBack);

    /**
     * @param messageCenterInfo 数据
     */
    void pushMessage(MessageCenterInfo messageCenterInfo);

    /**
     * @param msgType 类型
     * @return 数据
     */
    MessageCenterInfo getMessage(final int msgType);

}
