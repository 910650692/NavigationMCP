package com.fy.navi.scene.dialog;

import android.content.Context;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.Window;
import android.view.WindowManager;

import com.android.utils.ConvertUtils;
import com.fy.navi.scene.databinding.DialogMsgTopBinding;
import com.fy.navi.ui.define.TripID;
import com.fy.navi.ui.dialog.BaseDialog;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/2/24
 */
public class MsgTopDialog extends BaseDialog<DialogMsgTopBinding> {

    private TripID mTripId;
    public MsgTopDialog(Context context, TripID tripID) {
        super(context);
        mTripId = tripID;
    }

    public MsgTopDialog(Context context, TripID tripID, IBaseDialogClickListener dialogClickListener) {
        super(context);
        mTripId = tripID;
        setDialogClickListener(dialogClickListener);
    }

    @Override
    protected DialogMsgTopBinding initLayout() {
        return DialogMsgTopBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void initListener() {
        mViewBinding.topDialogView.setOnClickListener(v -> {
            cancel();
            if (null != mDialogClickListener) mDialogClickListener.onCommitClick(mTripId);
        });
        mViewBinding.topDialogImgClose.setOnClickListener(v -> cancel());
    }

    public MsgTopDialog setTitle(String text) {
        if (!ConvertUtils.isEmpty(text))
            mViewBinding.topDialogTitle.setText(text);
        return this;
    }

    public MsgTopDialog setContent(String text) {
        if (!ConvertUtils.isEmpty(text))
            mViewBinding.topDialogContent.setText(text);
        return this;
    }

    public MsgTopDialog setOutsideCancel(boolean cancel) {
        outsideCancel = cancel;
        return this;
    }

    public void showDialog() {
        super.showDialog(Gravity.TOP);
        Window window = getWindow();
        if (null == window) return;
        window.setLayout(getContext().getResources().getDimensionPixelOffset(com.fy.navi.ui.R.dimen.dp_948),
                WindowManager.LayoutParams.WRAP_CONTENT);
        WindowManager.LayoutParams params = window.getAttributes();
        window.setAttributes(params);
    }
}
