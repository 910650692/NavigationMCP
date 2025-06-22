package com.sgm.navi.scene.ui.poi;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;

import com.sgm.navi.scene.databinding.ChargeStationConfirmDialogBinding;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

public class ChargeStationConfirmDialog extends BaseFullScreenDialog<ChargeStationConfirmDialogBinding> {
    private String mTitle;
    private String mTip;
    private String mConfirmTitle;
    private String mCancelTitle;

    protected ChargeStationConfirmDialog(final Context context, final String title, final String tip,
                                  final String confirmTitle,final String cancelTitle, final IBaseDialogClickListener observer) {
        super(context);
        this.mTitle = title;
        this.mTip = tip;
        this.mConfirmTitle = confirmTitle;
        this.mCancelTitle = cancelTitle;
        mDialogClickListener = observer;
    }

    @Override
    protected ChargeStationConfirmDialogBinding initLayout() {
        return ChargeStationConfirmDialogBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewBinding.tvTitle.setText(mTitle);
        mViewBinding.tvTip.setText(mTip);
        onClick();
    }

    /**
     * 设置点击事件
     */
    public void onClick() {
        mViewBinding.tvCancel.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) {
                mDialogClickListener.onCancelClick();
            }
        });
        mViewBinding.tvConfirm.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) {
                mDialogClickListener.onCommitClick();
            }
        });
    }

    public static class Build {
        private final Context mContext;
        private String mTitle;
        private String mTip;
        private String mConfirmTitle;
        private String mCancelTitle;
        private IBaseDialogClickListener mDialogObserver;

        public Build(final Context context) {
            this.mContext = context;
        }

        /**
         * 设置标题
         * @param title 标题文本
         * @return Build
         */
        public ChargeStationConfirmDialog.Build setTitle(final String title) {
            this.mTitle = title;
            return this;
        }

        /**
         * 设置确认按钮标题文本
         * @param confirmTitle  确认按钮标题文本
         * @return Build
         */
        public ChargeStationConfirmDialog.Build setConfirmTitle(final String confirmTitle) {
            this.mConfirmTitle = confirmTitle;
            return this;
        }

        public ChargeStationConfirmDialog.Build setCancelTitle(final String cancelTitle) {
            this.mCancelTitle = cancelTitle;
            return this;
        }

        /**
         * 设置内容
         * @param tip 提示信息
         * @return Build
         */
        public ChargeStationConfirmDialog.Build setTip(final String tip) {
            this.mTip = tip;
            return this;
        }

        /**
         * 设置点击事件监听
         * @param dialogObserver 点击事件监听
         * @return Build
         */
        public ChargeStationConfirmDialog.Build setDialogObserver(final IBaseDialogClickListener dialogObserver) {
            this.mDialogObserver = dialogObserver;
            return this;
        }

        /**
         * 构造方法
         * @return SearchConfirmDialog对象
         */
        public ChargeStationConfirmDialog build() {
            return new ChargeStationConfirmDialog(mContext, mTitle, mTip, mConfirmTitle, mCancelTitle,mDialogObserver);
        }
    }
}
