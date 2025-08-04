package com.sgm.navi.hmi.startup;

import android.content.Context;
import android.os.Bundle;

import com.android.utils.NetWorkUtils;
import com.sgm.navi.hmi.databinding.DialogStartupExceptionBinding;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

public class StartupExceptionDialog extends BaseFullScreenDialog<DialogStartupExceptionBinding> {

    public StartupExceptionDialog(Context context, IBaseDialogClickListener baseDialogClickListener) {
        super(context);
        setDialogClickListener(baseDialogClickListener);
    }

    @Override
    protected DialogStartupExceptionBinding initLayout() {
        return DialogStartupExceptionBinding.inflate(getLayoutInflater());
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewBinding.tvExit.setOnClickListener(v -> {
            if (mDialogClickListener != null) {
                mDialogClickListener.onExit();
            }
        });
    }

    @Override
    public void dismiss() {
        super.dismiss();
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        setDialogClickListener(null);
    }
}
