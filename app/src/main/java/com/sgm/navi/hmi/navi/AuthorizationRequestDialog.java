package com.sgm.navi.hmi.navi;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.LayoutInflater;

import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.DialogAuthorizationRequestBinding;
import com.sgm.navi.ui.dialog.BaseDialog;

public class AuthorizationRequestDialog extends BaseDialog<DialogAuthorizationRequestBinding> {

    private final Context mContext;
    public AuthorizationRequestDialog(final Context context) {
        super(context);
        mContext = context;
    }

    @Override
    protected DialogAuthorizationRequestBinding initLayout() {
        return DialogAuthorizationRequestBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setCanceledOnTouchOutside(false);
    }

    @Override
    protected void initListener() {
        mViewBinding.dialogConfirm.setOnClickListener(v -> {
            cancel();
            if (mDialogClickListener != null) {
                mDialogClickListener.onCommitClick();
            }
        });

        mViewBinding.dialogCancel.setOnClickListener(v -> {
            cancel();
            System.exit(0);
        });
    }

    /**
     * 设置弹框内容
     * @param endDate
     */
    public void setEndDate(final String endDate) {
        if (!TextUtils.isEmpty(endDate)) {
            mViewBinding.dialogContent.setText(mContext.getString(R.string.authorization_request_content_renew, endDate));
        } else {
            mViewBinding.dialogContent.setText(R.string.authorization_request_content_first);
        }
    }
}
