package com.sgm.navi.service.adapter.message;

import com.sgm.navi.service.define.message.MessageCenterInfo;

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
