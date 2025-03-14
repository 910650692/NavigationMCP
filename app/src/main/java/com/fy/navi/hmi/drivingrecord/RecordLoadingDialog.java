package com.fy.navi.hmi.drivingrecord;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;

import com.fy.navi.hmi.databinding.DialogLoadingBinding;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

/**
 * 行程历史数据详情加载 dialog
 */
public class RecordLoadingDialog extends BaseFullScreenDialog<DialogLoadingBinding> {

    private final String content;

    @Override
    protected DialogLoadingBinding initLayout() {
        return DialogLoadingBinding.inflate(LayoutInflater.from(getContext()));
    }

    protected RecordLoadingDialog(Context context, String content, IBaseDialogClickListener observer) {
        super(context);
        this.content = content;
        mDialogClickListener = observer;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        mViewBinding.loadingContent.setText(content);

        mViewBinding.loadingClose.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) mDialogClickListener.onCancelClick();
        });

    }

    public static class Build {

        private final Context context;
        private String content;
        private IBaseDialogClickListener dialogObserver;

        public Build(Context context) {
            this.context = context;
        }

        public Build setContent(String content) {
            this.content = content;
            return this;
        }

        public Build setDialogObserver(IBaseDialogClickListener dialogObserver) {
            this.dialogObserver = dialogObserver;
            return this;
        }

        public RecordLoadingDialog build() {
            return new RecordLoadingDialog(context, content, dialogObserver);
        }
    }
}
