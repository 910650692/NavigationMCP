package com.sgm.navi.service.utils;


import android.content.Context;

import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.patac.hmi.privacy.domain.repository.AgreementDataSourceRepository;
import com.patac.hmi.privacy.domain.utils.DialogType;
import com.patac.hmi.privacy.model.proxy.IAgreementCallBack;
import com.sgm.navi.service.AppCache;

import java.util.Hashtable;

/**
 * @author: ZhangXiWei
 * $Revision.1.0\$
 * Date: 2025/6/26
 * Description: [对接大协议]
 */
public class AgreementManager implements IAgreementCallBack {

    private final String TAG = AgreementManager.class.getSimpleName();
    private Context mContext;
    private String mPackageName;
    private AgreementDataSourceRepository mAgreementDataSourceRepository;
    private Boolean mAgreementState;
    private final Hashtable<String, AgreementManagerCallback> mAgreementManagerCallbackList;

    public static AgreementManager getInstance() {
        return InstanceHolder.INSTANCE;
    }

    private static final class InstanceHolder {
        static final AgreementManager INSTANCE = new AgreementManager();
    }

    private AgreementManager() {
        mAgreementDataSourceRepository = AgreementDataSourceRepository.getInstance();
        mContext = AppCache.getInstance().getMContext();
        mAgreementManagerCallbackList = new Hashtable<>();
    }

    /**
     * 隐私协议管理初始化.
     */
    public void init() {
        mPackageName = mContext.getPackageName();
        Logger.d(TAG, "AgreementManager init", mPackageName);
        try {
            mAgreementDataSourceRepository.registerAgreementListener(
                    AgreementManager.this, mContext);
            if (DeviceUtils.isCar(mContext)) {
                mAgreementState = mAgreementDataSourceRepository.agreementStatus();
            } else {
                Logger.d(TAG, "AgreementManager init isCar");
                mAgreementState = true;
            }
            Logger.d(TAG, "AgreementManager init mAgreementState = ", mAgreementState);
            mAgreementDataSourceRepository.registerGmBaiduAgreementCallback(this);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    @Override
    public void agreementCallBac(boolean isSGMAgreed) {
        //isSGMAgreed SGM协议状态
        Logger.e(TAG, "isSGMAgreed=", isSGMAgreed);
        mAgreementState = isSGMAgreed;
        for (AgreementManagerCallback callback : mAgreementManagerCallbackList.values()) {
            callback.agreementCallback(isSGMAgreed);
        }
    }

    /**
     * 设置是否同意了SGM协议.
     *
     * @param state 是否同意协议.
     */
    public void allowSGMAgreement(boolean state) {
        Logger.i(TAG, "allowSGMAgreement: state = ", state);
        mAgreementDataSourceRepository.allowSGMAgreement(DialogType.GM_NORMAL_SIMPLE_DIALOG.getType(),
                mPackageName, state);
    }

    /**
     * 获取是否同意了SGM协议.
     *
     * @return  是否同意协议.
     */
    public boolean isAllowSGMAgreement() {
        if (DeviceUtils.isCar(mContext)) {
            mAgreementState = mAgreementDataSourceRepository.agreementStatus();
        } else {
            mAgreementState = true;
        }
        Logger.i(TAG, "isAllowSGMAgreement: ", mAgreementState);
        return mAgreementState;
    }

    /**
     * 打开SGM协议弹窗.
     *
     * @return 是否同意协议.
     */
    public boolean showSGMAgreement(boolean show) {
        mAgreementState = mAgreementDataSourceRepository.agreementStatus(mContext, show);
        Logger.i(TAG, "showSGMAgreement: ", mAgreementState);
        return mAgreementState;
    }

    public interface AgreementManagerCallback {
        /**
         * @param isSGMAgreed   SGM协议状态
         */
        default void agreementCallback(boolean isSGMAgreed) {

        }
    }

    /**
     * 监听设置项实时变化
     *
     * @param key      回调key
     * @param callback 回调
     */
    public synchronized void setAgreementCallback(final String key, final AgreementManagerCallback callback) {
        if (callback != null && !mAgreementManagerCallbackList.contains(callback)) {
            mAgreementManagerCallbackList.put(key, callback);
        }
    }

    /**
     * 注销回调
     * @param key 回调key
     */
    public void unRegisterAgreementCallback(final String key) {
        mAgreementManagerCallbackList.remove(key);
    }

    public void onDestroy(){
        mAgreementDataSourceRepository.unregisterGmBaiduAgreementCallback(this);
    }

}
