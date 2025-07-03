package com.sgm.navi.service.logicpaket.agreement;

import com.android.utils.log.Logger;
import com.sgm.navi.service.utils.AgreementManager;

import java.util.Hashtable;

public class AgreementPackage {

    public static final String TAG = AgreementPackage.class.getSimpleName();

    private final AgreementManager mAgreementManager;
    private final Hashtable<String, AgreementCallback> mAgreementCallbackList;

    public static AgreementPackage getInstance() {
        return AgreementPackage.SInstanceHolder.INSTANCE;
    }

    private static final class SInstanceHolder {
        static final AgreementPackage INSTANCE = new AgreementPackage();
    }

    private AgreementPackage() {
        Logger.d(TAG, "AgreementPackage: ");
        mAgreementCallbackList = new Hashtable<>();
        mAgreementManager = AgreementManager.getInstance();
        mAgreementManager.init();
    }

    /**
     * 监听设置项实时变化
     *
     * @param key      回调key
     * @param callback 回调
     */
    public synchronized void setAgreementCallback(final String key, final AgreementCallback callback) {
        if (callback != null && !mAgreementCallbackList.contains(callback)) {
            mAgreementCallbackList.put(key, callback);
        }
    }

    /**
     * 注销回调
     * @param key 回调key
     */
    public void unRegisterAgreementCallback(final String key) {
        mAgreementCallbackList.remove(key);
    }

    /**
     * 设置初始化
     */
    public void init() {
        Logger.d(TAG, "AgreementPackage: init");
        mAgreementManager.setAgreementCallback("AgreementPackage",
                new AgreementManager.AgreementManagerCallback() {
                    @Override
                    public void agreementCallback(boolean isSGMAgreed) {
                        Logger.d(TAG, "agreementCallback: isSGMAgreed = ", isSGMAgreed);
                        for (AgreementCallback callback : mAgreementCallbackList.values()) {
                            callback.agreementCallback(isSGMAgreed);
                        }
                    }
                });

    }

    /**
     * 设置是否同意了SGM协议.
     *
     * @param state 是否同意协议.
     */
    public void allowSGMAgreement(boolean state) {
        mAgreementManager.allowSGMAgreement(state);
    }

    /**
     * 获取是否同意了SGM协议.
     *
     * @return  是否同意协议.
     */
    public boolean isAllowSGMAgreement() {
        return mAgreementManager.isAllowSGMAgreement();
    }

    /**
     * 打开SGM协议弹窗.
     *
     * @return 是否同意协议.
     */
    public boolean showSGMAgreement(boolean show) {
        return mAgreementManager.showSGMAgreement(show);
    }

    public interface AgreementCallback {
        /**
         * @param isSGMAgreed   SGM协议状态
         */
        default void agreementCallback(boolean isSGMAgreed) {

        }
    }

    public void onDestroy(){
        Logger.d(TAG, "onDestroy: ");
        mAgreementManager.unRegisterAgreementCallback("SettingPackage");
        mAgreementManager.onDestroy();
    }
}
