package com.sgm.navi.hmi.drivingrecord;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;

import com.sgm.navi.hmi.databinding.DialogLoadingBinding;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;


public class RecordLoadingDialog extends BaseFullScreenDialog<DialogLoadingBinding> {

    private final String mContent;

    @Override
    protected DialogLoadingBinding initLayout() {
        return DialogLoadingBinding.inflate(LayoutInflater.from(getContext()));
    }

    protected RecordLoadingDialog(final Context context, final String content, final IBaseDialogClickListener observer) {
        super(context);
        this.mContent = content;
        mDialogClickListener = observer;
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        mViewBinding.loadingContent.setText(mContent);

        mViewBinding.loadingClose.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) {
                mDialogClickListener.onCancelClick();
            }
        });

    }

    public static class Build {

        private final Context mContext;
        private String mContent;
        private IBaseDialogClickListener mDialogObserver;

        public Build(final Context context) {
            this.mContext = context;
        }

        /**
         * set loading content
         * @param content loading content
         * @return Build
         */
        public Build setContent(final String content) {
            this.mContent = content;
            return this;
        }

        /**
         * set dialog observer
         * @param dialogObserver dialog observer
         * @return Build
         */
        public Build setDialogObserver(final IBaseDialogClickListener dialogObserver) {
            this.mDialogObserver = dialogObserver;
            return this;
        }

        /**
         * @return RecordLoadingDialog
         */
        public RecordLoadingDialog build() {
            return new RecordLoadingDialog(mContext, mContent, mDialogObserver);
        }
    }
}
