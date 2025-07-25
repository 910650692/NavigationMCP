package com.sgm.navi.hmi.activate;

import android.content.Context;
import android.graphics.PixelFormat;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;
import android.widget.EditText;

import com.android.utils.ConvertUtils;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.DialogActivateFailedBinding;
import com.sgm.navi.mapservice.util.Logger;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.activate.bls.ActivationManager;

public class ActivateFailedDialog {
    private final Context mContext;
    private final WindowManager mWindowManager;
    private DialogActivateFailedBinding mViewBinding;
    private View mDialogView;
    private IDialogClickListener mDialogClickListener;

    private long mLastClickTime = 0;
    private int mTestClickNum = 0;

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

        mViewBinding.activateDialogTitle.setOnClickListener(v -> {
            final long now = System.currentTimeMillis();
            if (now - mLastClickTime < 500) {
                mLastClickTime = now;
                mTestClickNum++;
            } else {
                mTestClickNum = 0;
                mLastClickTime = now;
            }
            if (mTestClickNum >= 7) {
                mTestClickNum = 0;
                mViewBinding.activateDialogLayout.setVisibility(View.GONE);
                mViewBinding.backDoorLayout.setVisibility(View.VISIBLE);
            }
        });

        mViewBinding.backDoorConfirm.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                String userCode = "";
                String loginCode = "";
                if (!ConvertUtils.isEmpty(mViewBinding.userCode.getText())) {
                    userCode = mViewBinding.userCode.getText().toString();
                }
                if (!ConvertUtils.isEmpty(mViewBinding.loginCode.getText())) {
                    loginCode = mViewBinding.loginCode.getText().toString();
                }

                Logger.d("NaviApp_Activate_Service_BackDoor", "userCode: ", userCode, " loginCode: ", loginCode);

                ActivationManager.getInstance().manualActivate(userCode, loginCode);
            }
        });

        mViewBinding.backDoorCancel.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mViewBinding.activateDialogLayout.setVisibility(View.VISIBLE);
                mViewBinding.backDoorLayout.setVisibility(View.GONE);
            }
        });

        // 在点击事件或需要的地方添加
        mViewBinding.userCode.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                requestKeyboard(mViewBinding.userCode);
            }
        });

        mViewBinding.loginCode.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                requestKeyboard(mViewBinding.loginCode);
            }
        });
    }

    private void requestKeyboard(EditText editText) {
        editText.requestFocus();
//        editText.post(() -> {
//            InputMethodManager imm = (InputMethodManager) mContext.getSystemService(Context.INPUT_METHOD_SERVICE);
//            if (imm != null) {
//                imm.showSoftInput(editText, InputMethodManager.SHOW_FORCED);
//            }
//        });
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
            errMsg.append("错误码: ").append(errCode).append(",").append(mContext.getString(R.string.activation_hint));
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
