package com.sgm.navi.service.logicpaket.message;

import com.sgm.navi.service.define.message.MessageCenterInfo;



public interface MessageCenterCallBack {

    /**
     * @param messageCenterInfo 数据
     */
    void onMessageInfoNotifyCallback(MessageCenterInfo messageCenterInfo);

    /**
     * 回调
     */
    void onMessageInfoRemoveCallback();

}
