package com.sgm.navi.hmi.mapdata;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.sgm.navi.hmi.databinding.DialogDownloadCountryBinding;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

public class DownloadCountryDialog extends BaseFullScreenDialog<DialogDownloadCountryBinding> {

    private final boolean mIsShowCancel;

    @Override
    protected DialogDownloadCountryBinding initLayout() {
        return DialogDownloadCountryBinding.inflate(LayoutInflater.from(getContext()));
    }

    protected DownloadCountryDialog(final Context context, final boolean isShowCancel,
                                    final IBaseDialogClickListener observer) {
        super(context);
        this.mIsShowCancel = isShowCancel;
        mDialogClickListener = observer;
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (!mIsShowCancel) {
            mViewBinding.dialogCancel.setVisibility(View.GONE);
        }

        mViewBinding.dialogCancel.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) {
                mDialogClickListener.onCancelClick();
            }
        });

        mViewBinding.dialogCommit.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener){
                mDialogClickListener.onCommitClick();
            }
        });
    }


    public static class Build {

        private final Context mContext;
        private boolean mIsShowCancel = true;
        private IBaseDialogClickListener mDialogObserver;

        public Build(final Context context) {
            this.mContext = context;
        }

        /**
         * 按钮点击状态监听
         * @param dialogObserver
         * @return 返回监听回调
         */
        public DownloadCountryDialog.Build setDialogObserver(final IBaseDialogClickListener dialogObserver) {
            this.mDialogObserver = dialogObserver;
            return this;
        }

        /**
         * 创建dialog
         * @return 返回dialog
         */
        public DownloadCountryDialog build() {
            return new DownloadCountryDialog(mContext, mIsShowCancel, mDialogObserver);
        }
    }

}
