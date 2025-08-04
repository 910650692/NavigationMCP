package com.sgm.navi.hmi.activate;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.logicpaket.activate.ActivatePackage;
import com.sgm.navi.service.logicpaket.activate.IActivateObserver;


public class ActivateUiStateManager implements StartService.ISdkInitCallback {

    private static final String TAG = MapDefaultFinalTag.ACTIVATE_SERVICE_TAG;
    private LoadingViewCallBack mActivateStateCallBack;
    private IActivateObserver mActObserver;

    private int mActivateState = AutoMapConstant.ActivateState.NONE;
    private int errorCodeSave = 0;
    private String errorMsgSave = "";

    private boolean isSafeToRegister = false;

    public boolean isSafeToRegister() {
        return isSafeToRegister;
    }

    private ActivateUiStateManager() {

    }

    public void init() {
        Logger.e(TAG, "ActivateUiStateManager init");
        isSafeToRegister = true;
        mActObserver = new IActivateObserver() {
            @Override
            public void onActivating() {
                Logger.e(TAG, "onActivating...");
                setActivateState(AutoMapConstant.ActivateState.ACTIVATING);
                if (mActivateStateCallBack != null) {
                    mActivateStateCallBack.onActivating();
                }
            }

            @Override
            public void onActivated() {
                Logger.e(TAG, "onActivated!");
                setActivateState(AutoMapConstant.ActivateState.ACTIVATED);
                if (mActivateStateCallBack != null) {
                    mActivateStateCallBack.onActivated();
                }
                ActivatePackage.getInstance().removeActObserver(this);
            }

            @Override
            public void onActivatedError(int errCode, String msg) {
                Logger.e(TAG, "激活出现错误 : ", errCode , "msg: ", msg);
                setActivateState(AutoMapConstant.ActivateState.ACTIVATE_FAILED);
                errorCodeSave = errCode;
                errorMsgSave = msg;
                if (mActivateStateCallBack != null) {
                    mActivateStateCallBack.onActivateFailed(errCode, msg);
                }
            }
        };
        ActivatePackage.getInstance().addActObserver(mActObserver);
    }

    /**
     * 如果是网络原因，重试激活
     */
    public void retryActivate() {
        Logger.e(TAG, "retryActivate: ", mActivateState);
        if (mActivateState == AutoMapConstant.ActivateState.ACTIVATE_FAILED) {
            ActivatePackage.getInstance().startActivate();
        }
    }

    public void setActivateStateCallBack(LoadingViewCallBack callBack) {
        this.mActivateStateCallBack = callBack;
    }

    public void removeActivateStateCallBack() {
        this.mActivateStateCallBack = null;
    }

    public String getErrorMsgSave() {
        return errorMsgSave;
    }

    public int getErrorCodeSave() {
        return errorCodeSave;
    }


    public boolean isActivating() {
        return ConvertUtils.equals(mActivateState, AutoMapConstant.ActivateState.ACTIVATING);
    }

    public boolean isActivateFailed() {
        return ConvertUtils.equals(mActivateState, AutoMapConstant.ActivateState.ACTIVATE_FAILED);
    }

    public int getActivateState() {
        return mActivateState;
    }

    public void setActivateState(int mActivateState) {
        this.mActivateState = mActivateState;
    }

    public interface LoadingViewCallBack {
        void onActivating();
        void onActivated();
        void onActivateFailed(final int errCode, final String msg);
    }

    private static class ActivateManagerHolder {
        private static final ActivateUiStateManager INSTANCE = new ActivateUiStateManager();
    }

    public static ActivateUiStateManager getInstance() {
        return ActivateManagerHolder.INSTANCE;
    }
}
