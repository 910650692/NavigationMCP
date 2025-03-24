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

public class MsgTopDialog extends BaseDialog<DialogMsgTopBinding> {

    private TripID mTripId;

    public MsgTopDialog(final Context context, final TripID tripID) {
        super(context);
        mTripId = tripID;
    }

    public MsgTopDialog(final Context context, final TripID tripID, final IBaseDialogClickListener dialogClickListener) {
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
            if (null != mDialogClickListener) {
                mDialogClickListener.onCommitClick(mTripId);
            }
        });
        mViewBinding.topDialogImgClose.setOnClickListener(v -> cancel());
    }

    /**
     * 设置标题
     *
     * @param text 标题
     * @return MsgTopDialog
     */
    public MsgTopDialog setTitle(final String text) {
        if (!ConvertUtils.isEmpty(text)) {
            mViewBinding.topDialogTitle.setText(text);
        }
        return this;
    }

    /**
     * 设置内容
     *
     * @param text 内容
     * @return MsgTopDialog
     */
    public MsgTopDialog setContent(final String text) {
        if (!ConvertUtils.isEmpty(text)) {
            mViewBinding.topDialogContent.setText(text);
        }
        return this;
    }

    /**
     * 设置点击其他区域关闭
     *
     * @param cancel 是否设置该状态
     * @return MsgTopDialog
     */
    public MsgTopDialog setOutsideCancel(final boolean cancel) {
        mOutsideCancel = cancel;
        return this;
    }

    /**
     * 展示弹框
     */
    public void showDialog() {
        super.showDialog(Gravity.TOP);
        final Window window = getWindow();
        if (null == window) {
            return;
        }
        window.setLayout(getContext().getResources().getDimensionPixelOffset(com.fy.navi.ui.R.dimen.dp_948),
                WindowManager.LayoutParams.WRAP_CONTENT);
        final WindowManager.LayoutParams params = window.getAttributes();
        window.setAttributes(params);
    }
}
