package com.sgm.navi.hmi.startup;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;

import com.sgm.navi.hmi.databinding.DialogActivateFailedBinding;
import com.sgm.navi.ui.dialog.BaseDialog;

public class ActivateFailedDialog extends BaseDialog<DialogActivateFailedBinding> {
    private final Context mContext;
    public ActivateFailedDialog(final Context context) {
        super(context);
        mContext = context;
    }
    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setOnCancelListener(null);
    }
    @Override
    protected DialogActivateFailedBinding initLayout() {
        return DialogActivateFailedBinding.inflate(LayoutInflater.from(getContext()));
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

}
