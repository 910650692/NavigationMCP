package com.sgm.navi.hmi.activate;

import android.content.Context;
import android.graphics.PixelFormat;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;

import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.DialogActivateFailedBinding;
import com.sgm.navi.mapservice.util.Logger;
import com.sgm.navi.service.MapDefaultFinalTag;

public class ActivateFailedDialog {
    private final Context mContext;
    private final WindowManager mWindowManager;
    private DialogActivateFailedBinding mViewBinding;
    private View mDialogView;
    private IDialogClickListener mDialogClickListener;

    public interface IDialogClickListener {
        void onCommitClick();

        void onCancelClick();
    }

    public ActivateFailedDialog(final Context context) {
        mContext = context;
        mWindowManager = (WindowManager) mContext.getSystemService(Context.WINDOW_SERVICE);
        mViewBinding = DialogActivateFailedBinding.inflate(LayoutInflater.from(mContext));
    }

    public void show() {
        if (mDialogView != null) {
            return;
        }
        createAndShowDialog();
    }

    private void createAndShowDialog() {
        mDialogView = mViewBinding.getRoot();
        WindowManager.LayoutParams params = new WindowManager.LayoutParams(
                WindowManager.LayoutParams.WRAP_CONTENT,
                WindowManager.LayoutParams.WRAP_CONTENT,
                WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY,
                WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE |
                        WindowManager.LayoutParams.FLAG_SHOW_WHEN_LOCKED |
                        WindowManager.LayoutParams.FLAG_DISMISS_KEYGUARD,
                PixelFormat.TRANSLUCENT
        );

        params.gravity = Gravity.CENTER;
        params.width = (int) (mContext.getResources().getDisplayMetrics().widthPixels * 0.8);

        try {
            mWindowManager.addView(mDialogView, params);
            initListener();
        } catch (Exception e) {
            Logger.e(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "Failed to add dialog view: " + e.getMessage());
        }
    }

    private void initListener() {
        if (mViewBinding == null) return;

        mViewBinding.activateDialogConfirm.setOnClickListener(v -> {
            dismiss();
            if (mDialogClickListener != null) {
                mDialogClickListener.onCommitClick();
            }
        });

        mViewBinding.activateDialogCancel.setOnClickListener(v -> {
            dismiss();
            if (mDialogClickListener != null) {
                mDialogClickListener.onCancelClick();
            }
        });
    }

    public void dismiss() {
        if (mDialogView != null && mWindowManager != null) {
            Logger.d(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "Dismissing dialog view");
            try {
                mWindowManager.removeView(mDialogView);
            } catch (Exception e) {
                Logger.e(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "Failed to remove dialog view: " + e.getMessage());
            }
            mDialogView = null;
            mViewBinding = null;
        }
    }

    /**
     * 更改弹窗文本
     *
     * @param isNetError 是否是网络有问题导致的
     */
    public void changeDialogContent(final int errCode, final boolean isNetError) {
        if (isNetError) {
            mViewBinding.activateDialogContent.setText(R.string.activation_net_error_hint);
        } else {
            StringBuilder errMsg = new StringBuilder();
            errMsg.append("错误码: ").append(errCode).append(" ").append(mContext.getString(R.string.activation_hint));
            mViewBinding.activateDialogContent.setText(errMsg);
        }
    }

    public void setDialogClickListener(IDialogClickListener listener) {
        mDialogClickListener = listener;
    }

    public boolean isShowing() {
        return mDialogView != null;
    }

}
