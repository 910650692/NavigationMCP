package com.fy.navi.service.adapter.message;

import com.fy.navi.service.define.message.MessageCenterInfo;

public interface MessageCenterAdapterCallBack {

    /**
     * @param messageCenterInfo 数据
     */
    void onMessageInfoNotifyCallback(MessageCenterInfo messageCenterInfo);

    /**
     *  移除数据
     */
    void onMessageInfoRemoveCallback();

}
