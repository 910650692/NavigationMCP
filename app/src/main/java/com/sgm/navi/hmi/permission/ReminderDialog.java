package com.sgm.navi.hmi.permission;

import android.content.Context;
import android.os.Bundle;
import android.view.View;

import androidx.core.widget.NestedScrollView;

import com.android.utils.ConvertUtils;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.DialogUseReminderBinding;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/31
 */
public class ReminderDialog extends BaseFullScreenDialog<DialogUseReminderBinding> {
    public ReminderDialog(Context context, IBaseDialogClickListener baseDialogClickListener) {
        super(context);
        setDialogClickListener(baseDialogClickListener);
    }

    @Override
    protected DialogUseReminderBinding initLayout() {
        return DialogUseReminderBinding.inflate(getLayoutInflater());
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setCancelable(false);
        mViewBinding.reminderIndex.reminderTermsService.setOnClickListener(new View.OnClickListener() {
            @Override
            @HookMethod(eventName = BuryConstant.EventName.AMAP_SERVICEAGREEMENT_CHECK)
            public void onClick(View v) {
                showOrHideDetail(true);
                mViewBinding.reminderDetail.reminderTitle.setText(R.string.reminder_page_service_title);
                mViewBinding.reminderDetail.reminderContent.setText(R.string.reminder_page_service_content);
            }
        });
        mViewBinding.reminderIndex.reminderPagePrivacy.setOnClickListener(v -> {
            showOrHideDetail(true);
            mViewBinding.reminderDetail.reminderTitle.setText(R.string.reminder_page_privacy_title);
            mViewBinding.reminderDetail.reminderContent.setText(R.string.reminder_page_privacy_content);
        });
        mViewBinding.reminderIndex.dialogCommit.setOnClickListener(v -> {
            if (mDialogClickListener != null) {
                mDialogClickListener.onCommitClick();
            }
            cancel();
        });
        mViewBinding.reminderIndex.dialogCancel.setOnClickListener(v -> {
            if (mDialogClickListener != null) {
                mDialogClickListener.onCancelClick();
            }
            cancel();
        });

        mViewBinding.reminderDetail.reminderIvBack.setOnClickListener(v -> {
            showOrHideDetail(false);
        });
    }

    private void showOrHideDetail(boolean isShow) {
        if (isShow) {
            mViewBinding.reminderIndex.reminderRootIndex.setVisibility(View.INVISIBLE);
            mViewBinding.reminderDetail.reminderRootDetail.setVisibility(View.VISIBLE);
        } else {
            mViewBinding.reminderIndex.reminderRootIndex.setVisibility(View.VISIBLE);
            mViewBinding.reminderDetail.reminderRootDetail.setVisibility(View.INVISIBLE);
            resetScrollPosition();
        }
    }

    private void resetScrollPosition() {
        NestedScrollView scrollView = (NestedScrollView) mViewBinding.reminderDetail.reminderContent.getParent();
        if (!ConvertUtils.isNull(scrollView)) {
            scrollView.scrollTo(0, 0);
        }
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        setDialogClickListener(null);
    }
}
