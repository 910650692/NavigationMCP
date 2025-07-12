package com.sgm.navi.ui.dialog;

import android.app.Dialog;
import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.Window;
import android.view.WindowManager;

import androidx.annotation.GravityInt;
import androidx.databinding.ViewDataBinding;

import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;
import com.android.utils.process.ProcessStatus;

import java.util.Objects;

public abstract class BaseDialog<V extends ViewDataBinding> extends Dialog {
    private static final String TAG = BaseDialog.class.getSimpleName();
    protected IBaseDialogClickListener mDialogClickListener;
    protected V mViewBinding;
    protected Drawable mBackgroundColor;
    /*** 点击空白处是否取消Dialog false:不结束，true:结束 **/
    protected boolean mOutsideCancel = false;

    protected BaseDialog(final Context context) {
        super(context);
        mViewBinding = initLayout();
    }

    public void setBackgroundColor(final Drawable backgroundColor) {
        this.mBackgroundColor = backgroundColor;
    }

    public void setDialogClickListener(final IBaseDialogClickListener dialogClickListener) {
        this.mDialogClickListener = dialogClickListener;
    }

    protected abstract V initLayout();

    protected abstract void initListener();

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (null == mBackgroundColor) {
            mBackgroundColor = new ColorDrawable(Color.TRANSPARENT);
        }
        Objects.requireNonNull(getWindow()).setBackgroundDrawable(mBackgroundColor);
        setContentView(mViewBinding.getRoot());
        setOnShowListener(dialog -> {
            if (null != mDialogClickListener) {
                mDialogClickListener.onShowListener();
            }
        });
        setOnCancelListener(dialog -> {
            if (null != mDialogClickListener) {
                mDialogClickListener.onCancelClick();
            }
        });
        initListener();
    }

    /**
     * 显示Dialog
     *
     * @param position 显示位置
     */
    public void showDialog(final @GravityInt int position) {
        try {
            int appRunStatus = ProcessManager.getAppRunStatus();
            Logger.d(TAG, "showDialog: appRunStatus = " + appRunStatus);
            if(appRunStatus == ProcessStatus.AppRunStatus.PAUSED
                    || appRunStatus == ProcessStatus.AppRunStatus.STOPPED
                    || appRunStatus == ProcessStatus.AppRunStatus.DESTROYED){
                return;
            }
            super.show();
            setCancelable(mOutsideCancel);
            final Window window = getWindow();
            if (null == window) {
                return;
            }
            window.setLayout(WindowManager.LayoutParams.WRAP_CONTENT, WindowManager.LayoutParams.WRAP_CONTENT);
            final WindowManager.LayoutParams params = window.getAttributes();
            params.gravity = position;
            window.setAttributes(params);
            window.addFlags(WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL);
            window.clearFlags(WindowManager.LayoutParams.FLAG_DIM_BEHIND);
//        window.setWindowAnimations(R.style.DialogBottomAnimation);
        } catch (WindowManager.BadTokenException e) {
            // 捕获异常，通常是因为Activity已经销毁
            Logger.d(TAG, "showDialog: ", e.getMessage());
        }
    }

    @Override
    public boolean isShowing() {
        int appRunStatus = ProcessManager.getAppRunStatus();
        Logger.d(TAG, "showDialog: appRunStatus = " + appRunStatus);
        if(appRunStatus == ProcessStatus.AppRunStatus.PAUSED
                || appRunStatus == ProcessStatus.AppRunStatus.STOPPED
                || appRunStatus == ProcessStatus.AppRunStatus.DESTROYED){
            return true;
        }
        return super.isShowing();
    }

    @Override
    public void hide() {
        int appRunStatus = ProcessManager.getAppRunStatus();
        Logger.d(TAG, "showDialog: appRunStatus = " + appRunStatus);
        if(appRunStatus == ProcessStatus.AppRunStatus.PAUSED
                || appRunStatus == ProcessStatus.AppRunStatus.STOPPED
                || appRunStatus == ProcessStatus.AppRunStatus.DESTROYED){
            return;
        }
        if (isShowing()) super.hide();
    }

    @Override
    public void dismiss() {
        int appRunStatus = ProcessManager.getAppRunStatus();
        Logger.d(TAG, "showDialog: appRunStatus = " + appRunStatus);
        if(appRunStatus == ProcessStatus.AppRunStatus.PAUSED
                || appRunStatus == ProcessStatus.AppRunStatus.STOPPED
                || appRunStatus == ProcessStatus.AppRunStatus.DESTROYED){
            return;
        }
        if (isShowing()) super.dismiss();
    }

    @Override
    public void cancel() {
        int appRunStatus = ProcessManager.getAppRunStatus();
        Logger.d(TAG, "showDialog: appRunStatus = " + appRunStatus);
        if(appRunStatus == ProcessStatus.AppRunStatus.PAUSED
                || appRunStatus == ProcessStatus.AppRunStatus.STOPPED
                || appRunStatus == ProcessStatus.AppRunStatus.DESTROYED){
            return;
        }
        if (isShowing()) super.cancel();
    }
}
