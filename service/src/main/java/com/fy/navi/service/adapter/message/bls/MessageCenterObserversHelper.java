package com.fy.navi.service.adapter.message.bls;

import com.fy.navi.service.adapter.message.MessageCenterAdapterCallBack;
import java.util.Hashtable;

public class MessageCenterObserversHelper {

    private final Hashtable<String, MessageCenterAdapterCallBack> messageCenterAdapterCallBackHashtable;

    public MessageCenterObserversHelper() {
        messageCenterAdapterCallBackHashtable = new Hashtable<>();
    }

    /**
     * @param key key
     * @param callBack 回调
     */
    public void registerCallBack(final String key,final MessageCenterAdapterCallBack callBack) {
        messageCenterAdapterCallBackHashtable.put(key,callBack);
    }

    /**
     * @param key key
     * @param callBack 回调
     */
    public void unRegisterCallback(final String key,final MessageCenterAdapterCallBack callBack) {
        messageCenterAdapterCallBackHashtable.remove(key,callBack);
    }
}
