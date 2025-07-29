package com.sgm.navi.hmi.navi;

import android.content.Context;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.Window;
import android.view.WindowManager;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.BuildConfig;
import com.sgm.navi.hmi.databinding.DialogPhoneAddressBinding;
import com.sgm.navi.ui.dialog.BaseDialog;

public class PhoneAddressDialog extends BaseDialog<DialogPhoneAddressBinding> {

    public static final int SHOW_TIME = 8000;

    public PhoneAddressDialog(final Context context) {
        super(context);
        ThreadManager.getInstance().postDelay(this::dismiss, SHOW_TIME);
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

        mViewBinding.ivClose.setOnClickListener(v -> {
            dismiss();
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
    public void showDialog(boolean isMainBTNVisible) {
        super.showDialog(Gravity.TOP | Gravity.START);
        final Window window = getWindow();
        if (null == window) {
            return;
        }
        window.setLayout(WindowManager.LayoutParams.WRAP_CONTENT,
                WindowManager.LayoutParams.WRAP_CONTENT);
        final WindowManager.LayoutParams params = window.getAttributes();
        if (BuildConfig.FLAVOR.equals("clea_local_8155") || BuildConfig.FLAVOR.equals("clea_8775")) {
            if (isMainBTNVisible) {
                params.x = ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_phone_address_dialog_width);
            } else {
                params.x = ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_phone_address_dialog_width_main_btn);
            }
            params.y = ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_main_tap_margin_top);
        } else {
            if (isMainBTNVisible) {
                params.x = ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_phone_address_dialog_width);
            } else {
                params.x = ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_phone_address_dialog_width_main_btn);
            }
            params.y = ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_main_tap_margin_top);
        }
        window.setAttributes(params);
    }

    public void resetDialogParams(int x){
        final Window window = getWindow();
        if (window != null) {
            WindowManager.LayoutParams params = window.getAttributes();
            params.x = x;
            window.setAttributes(params);
            window.getDecorView().requestLayout();
        }
    }
}
