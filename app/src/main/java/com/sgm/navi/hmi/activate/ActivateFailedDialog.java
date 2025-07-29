package com.sgm.navi.hmi.activate;

import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.widget.EditText;

import com.android.utils.ConvertUtils;
import com.sgm.navi.hmi.databinding.DialogActivateFailedBinding;
import com.sgm.navi.mapservice.util.Logger;
import com.sgm.navi.service.adapter.activate.bls.ActivationManager;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

public class ActivateFailedDialog extends BaseFullScreenDialog<DialogActivateFailedBinding> {
    private Context mContext;

    private long mLastClickTime = 0;
    private int mTestClickNum = 0;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setCancelable(false);
        initListener();
    }

    public ActivateFailedDialog(final Context context, IBaseDialogClickListener baseDialogClickListener) {
        super(context);
        mContext = context;
        setDialogClickListener(baseDialogClickListener);
    }

    public void unInitContext() {
        if (mContext != null) {
            mContext = null;
        }
    }

    @Override
    protected DialogActivateFailedBinding initLayout() {
        return DialogActivateFailedBinding.inflate(getLayoutInflater());
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

                Logger.e("NaviApp_Activate_Service_BackDoor", "userCode: ", userCode, " loginCode: ", loginCode);

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

    /**
     * 更改弹窗文本
     *
     * @param errCode 错误码
     */
    public void changeDialogContent(final int errCode, final String msg) {
        StringBuilder errMsg = new StringBuilder();
        errMsg.append("错误码: ").append(errCode).append(",").append(msg);
        mViewBinding.activateDialogContent.setText(errMsg);
    }

}
