package com.fy.navi.hmi.activity;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;


import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.DialogNetActivateFailedBinding;
import com.fy.navi.ui.dialog.BaseDialog;

public class NetActivateFailedDialog extends BaseDialog<DialogNetActivateFailedBinding> {

    private final Context mContext;

    public NetActivateFailedDialog(final Context context) {
        super(context);
        mContext = context;
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setOnCancelListener(null);
    }

    @Override
    protected DialogNetActivateFailedBinding initLayout() {
        return DialogNetActivateFailedBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void initListener() {
        mViewBinding.dialogConfirm.setOnClickListener(v -> {
            dismiss();
            if (mDialogClickListener != null) {
                mDialogClickListener.onCommitClick();
            }
        });

        mViewBinding.dialogCancel.setOnClickListener(v -> {
            dismiss();
            if (mDialogClickListener != null) {
                mDialogClickListener.onCancelClick();
            }
        });
    }

    /**
     * 更改确认按钮文字
     */
    public void setConfirmText() {
        Logger.d("BaseStartupViewModel", "更改确认文字");
        mViewBinding.dialogConfirm.setText(R.string.activation_manual);
    }
}
