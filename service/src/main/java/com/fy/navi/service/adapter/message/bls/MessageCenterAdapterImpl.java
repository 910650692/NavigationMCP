package com.fy.navi.service.adapter.message.bls;

import android.os.CountDownTimer;
import com.android.utils.log.Logger;
import com.fy.navi.service.adapter.message.IMessageCenterApi;
import com.fy.navi.service.adapter.message.MessageCenterAdapterCallBack;
import com.fy.navi.service.define.message.MessageCenterInfo;
import java.util.Hashtable;
import java.util.LinkedList;

public class MessageCenterAdapterImpl implements IMessageCenterApi {

    private final Hashtable<String, MessageCenterAdapterCallBack> mAdapterImplCallBackHashtable;
    private final LinkedList<MessageCenterInfo> messageQuenu = new LinkedList<>();

    private CountDownTimer mCountDownTimer;//倒计时
    private final int mTOTALTIME = 8000;//总的倒计时时间
    private final int mSECONDTIME = 1000;//每间隔一秒进行回调

    public MessageCenterAdapterImpl(){
        mAdapterImplCallBackHashtable = new Hashtable<>();
    }

    @Override
    public void init() {

    }

    /**
     * @param key key
     * @param callBack 回调
     */
    @Override
    public void registerCallBack(final String key,final MessageCenterAdapterCallBack callBack) {
        //有可能不要helper
        mAdapterImplCallBackHashtable.put(key,callBack);
    }

    /**
     * @param key key
     * @param callBack 回调
     */
    @Override
    public void unRegisterCallback(final String key,final MessageCenterAdapterCallBack callBack) {
        mAdapterImplCallBackHashtable.remove(key,callBack);
    }


    /**
     * @param messageCenterInfo 回调
     */
    @Override
    public void pushMessage(final MessageCenterInfo messageCenterInfo) {
        //todo 可能还要加判重逻辑
        addMessage(messageCenterInfo);
        //可能需要 过滤 重复的消息展示
        if(!messageQuenu.isEmpty()){
            cancelTimeCountdown();
            startTimeCountdown();
            showMessageView(messageCenterInfo);
        }
    }

    /**
     * @param msgType 类型
     */
    @Override
    public MessageCenterInfo getMessage(final int msgType) {
        return null;
    }

    /**
     * @param messageCenterInfo 数据
     */
    private void addMessage(final MessageCenterInfo messageCenterInfo) {
        messageQuenu.add(messageCenterInfo);
    }

    /**
     * 8秒过后，删除队列的第一条消息
     */
    public void removeMessage() {
        messageQuenu.poll();
    }


    /**
     *  实现倒计时关闭消息view
     */
    private void startTimeCountdown(){
        mCountDownTimer = new CountDownTimer(mTOTALTIME,mSECONDTIME) {
            @Override
            public void onTick(final long millisUntilFinished) {

            }

            @Override
            public void onFinish() {
                if(!messageQuenu.isEmpty()){
                    removeMessage();
                    startTimeCountdown();
                    Logger.i("clickMessage","111--  "+messageQuenu.size());
                    if(messageQuenu.isEmpty()){
                        //通知隐藏view
                        removeMessageView();
                    }
                }
            }
        }.start();
    }

    /**
     *  取消倒计时
     */
    private void cancelTimeCountdown(){
        if(mCountDownTimer!=null){
            mCountDownTimer.cancel();
            mCountDownTimer = null;
        }
    }

    /**
     * @param messageCenterInfo 数据
     */
    private void showMessageView(final MessageCenterInfo messageCenterInfo) {
        for (MessageCenterAdapterCallBack resultCallback : mAdapterImplCallBackHashtable.values()) {
            resultCallback.onMessageInfoNotifyCallback(messageCenterInfo);
        }
    }

    /**
     *  隐藏消息view
     */
    private void removeMessageView() {
        cancelTimeCountdown();
        for (MessageCenterAdapterCallBack resultCallback : mAdapterImplCallBackHashtable.values()) {
            resultCallback.onMessageInfoRemoveCallback();
        }
    }
}
