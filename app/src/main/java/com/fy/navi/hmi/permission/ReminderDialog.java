package com.fy.navi.hmi.permission;

import android.content.Context;
import android.os.Bundle;
import android.view.View;

import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.DialogUseReminderBinding;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

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
        }
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        setDialogClickListener(null);
    }
}
