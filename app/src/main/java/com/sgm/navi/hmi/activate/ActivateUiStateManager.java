package com.sgm.navi.hmi.activate;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.logicpaket.activate.ActivatePackage;
import com.sgm.navi.service.logicpaket.activate.IActivateObserver;
import com.sgm.navi.ui.base.StackManager;

import java.util.concurrent.TimeUnit;


public class ActivateUiStateManager implements StartService.ISdkInitCallback {

    private static final String TAG = MapDefaultFinalTag.ACTIVATE_SERVICE_TAG;
    private ActivateFailedDialog mFailedDialog;
    private LoadingViewCallBack mLoadingViewCallBack;

    private IActivateObserver mActObserver;
    private boolean mIsActivating = false;

    private ActivateUiStateManager() {

    }

    public void init() {
        Logger.e(TAG, "ActivateUiStateManager init");
        mActObserver = new IActivateObserver() {
            @Override
            public void onActivating() {
                Logger.e(TAG, "onActivating...");
                if (mLoadingViewCallBack != null) {
                    mLoadingViewCallBack.showActivatingView(true);
                }
                mIsActivating = true;
            }

            @Override
            public void onActivated() {
                Logger.e(TAG, "onActivated!");
                if (mLoadingViewCallBack != null) {
                    mLoadingViewCallBack.showActivatingView(false);
                }
                dismissActivateFailedDialog();
                ActivatePackage.getInstance().removeActObserver(this);
                mIsActivating = false;
            }

            @Override
            public void onActivatedError(int errCode, String msg) {
                Logger.e(TAG, "激活出现错误 : ", errCode , "msg: ", msg);
                if (mLoadingViewCallBack != null) {
                    mLoadingViewCallBack.showActivatingView(false);
                }
                ThreadManager.getInstance().postUi(new Runnable() {
                    @Override
                    public void run() {
                        showActivateFailedDialog(errCode, msg);
                    }
                });
                mIsActivating = false;
            }
        };
        ActivatePackage.getInstance().addActObserver(mActObserver);
    }

    public void setLoadingViewCallBack(LoadingViewCallBack callBack) {
        if (mLoadingViewCallBack == null) {
            this.mLoadingViewCallBack = callBack;
        }
    }

    public void removeLoadingViewCallBack() {
        if (mLoadingViewCallBack != null) {
            this.mLoadingViewCallBack = null;
        }
    }

    public boolean ismIsActivating() {
        return mIsActivating;
    }
    /**
     * 显示激活失败弹窗
     *
     * @param msg 错误信息
     */
    public void showActivateFailedDialog(final int errCode, final String msg) {
        if (mFailedDialog != null && mFailedDialog.isShowing()) {
            Logger.e(TAG, "dialog showing");
            return;
        }
        Logger.e(TAG, "context: ", AppCache.getInstance().getMContext());
        mFailedDialog = new ActivateFailedDialog(AppCache.getInstance().getMContext());
        mFailedDialog.changeDialogContent(errCode, ConvertUtils.equals(20008, errCode));

        mFailedDialog.setDialogClickListener(new ActivateFailedDialog.IDialogClickListener() {
            @Override
            public void onCommitClick() {
                Logger.e(TAG, "重试激活");
                StartService.getInstance().startActivation();
            }

            @Override
            public void onCancelClick() {
                Logger.e(TAG, "激活失败,手动退出应用");
                ThreadManager.getInstance().asyncDelay(new Runnable() {
                    @Override
                    public void run() {
                        StackManager.getInstance().exitApp();
                    }
                }, 2, TimeUnit.MINUTES);
            }
        });

        mFailedDialog.show();
    }

    /**
     * 关闭弹窗
     */
    public void dismissActivateFailedDialog() {
        if (mFailedDialog != null && mFailedDialog.isShowing()) {
            mFailedDialog.dismiss();
            mFailedDialog = null;
        }
    }

    public interface LoadingViewCallBack {
        void showActivatingView(boolean isShow);
    }

    private static class ActivateManagerHolder {
        private static final ActivateUiStateManager INSTANCE = new ActivateUiStateManager();
    }

    public static ActivateUiStateManager getInstance() {
        return ActivateManagerHolder.INSTANCE;
    }
}
