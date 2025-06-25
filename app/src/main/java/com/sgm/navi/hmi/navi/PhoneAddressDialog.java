package com.sgm.navi.hmi.navi;

import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.Window;
import android.view.WindowManager;

import com.android.utils.ConvertUtils;
import com.sgm.navi.hmi.databinding.DialogPhoneAddressBinding;
import com.sgm.navi.ui.dialog.BaseDialog;

public class PhoneAddressDialog extends BaseDialog<DialogPhoneAddressBinding> {

    public static final int SHOW_TIME = 8000;
    private final Handler mHandler = new Handler(Looper.myLooper());

    public PhoneAddressDialog(final Context context) {
        super(context);
        mHandler.postDelayed(this::dismiss, SHOW_TIME);
    }

    @Override
    protected DialogPhoneAddressBinding initLayout() {
        return DialogPhoneAddressBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void initListener() {
        mViewBinding.dialogView.setOnClickListener(v -> {
            cancel();
            if (mDialogClickListener != null) {
                mDialogClickListener.onCommitClick();
            }
        });
    }

    /**
     * set title in dialog
     * @param title poi name
     * @return this
     */
    public PhoneAddressDialog setTitle(final String title) {
        if (!ConvertUtils.isEmpty(title)) {
            mViewBinding.addressTitle.setText(title);
        }
        return this;
    }

    /**
     * show dialog
     */
    public void showDialog() {
        super.showDialog(Gravity.TOP);
        final Window window = getWindow();
        if (null == window) {
            return;
        }
        window.setLayout(getContext().getResources().getDimensionPixelOffset(com.sgm.navi.ui.R.dimen.dp_948),
                WindowManager.LayoutParams.WRAP_CONTENT);
        final WindowManager.LayoutParams params = window.getAttributes();
        window.setAttributes(params);
    }
}
