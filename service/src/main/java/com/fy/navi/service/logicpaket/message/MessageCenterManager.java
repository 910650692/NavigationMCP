package com.fy.navi.service.logicpaket.message;

import android.os.CountDownTimer;
import com.android.utils.log.Logger;
import com.fy.navi.service.define.message.MessageCenterInfo;
import java.util.Hashtable;
import java.util.LinkedList;

/**
 * @Description:
 * @author: chao.tang
 * @date: 2025年03月29日 15:43
 */
public class MessageCenterManager {

    public static final String MESSAGECENTERKEY = "MessageCenterKey";

    private CountDownTimer mCountDownTimer;//倒计时
    private final int mTOTALTIME = 8000;//总的倒计时时间
    private final int mSECONDTIME = 1000;//每间隔一秒进行回调
    private final LinkedList<MessageCenterInfo> messageQuenu = new LinkedList<>();

    //1.先从package调取adapter
    final private MessageCenterManager messageCenterManager;
    private final Hashtable<String, MessageCenterCallBack> mCallBacks;

    private boolean isFirstCountdown = true;

    private MessageCenterManager() {
        messageCenterManager = MessageCenterManager.getInstance();
        mCallBacks = new Hashtable<>();
    }

    public static MessageCenterManager getInstance() {
        return MessageCenterManager.Helper.MESSAGEPACKAGE;
    }

    private static final class Helper {
        private static final MessageCenterManager MESSAGEPACKAGE = new MessageCenterManager();
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
        addMessage(messageCenterInfo);
        if(!messageQuenu.isEmpty()){
            cancelTimeCountdown();
            startTimeCountdown();
        }
    }

    /**
     * 点X删除消息
     */
    public void deleteMessage() {
        if(!messageQuenu.isEmpty()){
            messageQuenu.removeLast();
            cancelTimeCountdown();
            startTimeCountdown();
        }
    }

    /**
     * @param messageCenterInfo 数据
     */
    private void addMessage(final MessageCenterInfo messageCenterInfo) {
        messageQuenu.add(messageCenterInfo);
    }

    /**
     *  实现倒计时关闭消息view
     */
    private void startTimeCountdown(){
        isFirstCountdown = true;
        mCountDownTimer = new CountDownTimer(mTOTALTIME,mSECONDTIME) {
            @Override
            public void onTick(final long millisUntilFinished) {
                if(isFirstCountdown){
                    isFirstCountdown = false;
                    if(!messageQuenu.isEmpty()){
                        MessageCenterInfo messageCenterInfo = messageQuenu.getLast();
                        if(messageCenterInfo!=null){
                            showMessageView(messageCenterInfo);
                            Logger.i("onTick","222--  "+messageCenterInfo.getMsgType());
                        }
                    }
                }
            }

            @Override
            public void onFinish() {
                isFirstCountdown = true;
                if(!messageQuenu.isEmpty()){
                    removeMessage();
                    startTimeCountdown();
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
     * 8秒过后，删除队列的第一条消息
     */
    public void removeMessage() {
        if(!messageQuenu.isEmpty()){
            MessageCenterInfo info  = messageQuenu.removeLast();
            Logger.i("onFinish","111--  "+messageQuenu.size()+"删除了--"+info.getMsgType());
        }
    }

    /**
     * @param messageCenterInfo 数据
     */
    private void showMessageView(final MessageCenterInfo messageCenterInfo) {
        for (MessageCenterCallBack resultCallback : mCallBacks.values()) {
            resultCallback.onMessageInfoNotifyCallback(messageCenterInfo);
        }
    }

    /**
     *  隐藏消息view
     */
    private void removeMessageView() {
        cancelTimeCountdown();
        for (MessageCenterCallBack resultCallback : mCallBacks.values()) {
            resultCallback.onMessageInfoRemoveCallback();
        }
    }
}
