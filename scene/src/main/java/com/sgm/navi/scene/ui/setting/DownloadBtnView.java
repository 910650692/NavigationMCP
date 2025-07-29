package com.sgm.navi.scene.ui.setting;

import android.content.Context;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.sgm.navi.scene.R;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.mapdata.CityDownLoadInfo;
import com.sgm.navi.ui.view.SkinConstraintLayout;
import com.sgm.navi.ui.view.SkinLinearLayout;

public class DownloadBtnView extends SkinConstraintLayout {

    private SkinLinearLayout mLinearLayout;
    private ImageView mDownloadViewIcon;
    private TextView mDownloadViewText;
    private SkinConstraintLayout mDownloadViewStatus;

    /**
     * Constructor.
     *
     * @param context context
     * @param attrs   attributeSet
     */
    public DownloadBtnView(final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
        final View view = View.inflate(context, R.layout.download_view, this);
        mDownloadViewIcon = view.findViewById(R.id.btn_status);
        mDownloadViewText = view.findViewById(R.id.status_tip);
        mDownloadViewStatus = view.findViewById(R.id.download_status);
        mLinearLayout = view.findViewById(R.id.ll_root);
    }

    /**
     * 设置下载按钮提示icon
     *
     * @param res
     */
    private void setDownloadViewIcon(final int res) {
        mDownloadViewIcon.setImageResource(res);
    }

    /**
     * 设置下载按钮提示 icon 是否可见
     *
     * @param visible
     */
    private void setDownloadViewIconVisible(final boolean visible) {
        mDownloadViewIcon.setVisibility(visible ? View.VISIBLE : View.GONE);
    }

    /**
     * 设置下载按钮文字
     *
     * @param text
     */
    private void setDownloadViewText(final String text) {
        mDownloadViewText.setText(text);
    }

    /**
     * 设置下载按钮提示文字颜色
     *
     * @param res
     */
    private void setDownloadViewTextColor(final int res) {
        mDownloadViewText.setTextColor(getResources().getColor(res));
    }

    /**
     * 设置下载按钮背景
     *
     * @param res
     */
    private void setDownloadViewBackground(final int res) {
        mDownloadViewStatus.setBackgroundResource(res);
    }

    /**
     * Change UI display according to download status.
     *
     * @param data OfflineDatas
     */
    public void parseDownloadStatusInfo(final CityDownLoadInfo data) {
        switch (data.getTaskState()) {
            case UserDataCode.TASK_STATUS_CODE_READY:
                if (data.isUpdate()) {
                    setDownloadViewText("更新");
                    setDownloadViewIcon(R.drawable.img_refresh_bwhite_42);
                } else {
                    setDownloadViewText("下载");
                    setDownloadViewIcon(R.drawable.img_untop_bwhite_42);
                }
                setDownloadViewBackground(R.drawable.shape_bg_download_data);
                setDownloadViewTextColor(R.color.setting_white);
                setDownloadViewIconVisible(true);
                break;
            case UserDataCode.TASK_STATUS_CODE_WAITING:
                setDownloadViewText("等待中");
                setDownloadViewBackground(R.drawable.shape_bg_download_data);
                setDownloadViewTextColor(R.color.setting_white);
                setDownloadViewIconVisible(true);
                setDownloadViewIcon(R.drawable.img_suspend_42day);
                break;
            case UserDataCode.TASK_STATUS_CODE_PAUSE: // 继续
                setDownloadViewText("继续");
                setDownloadViewBackground(R.drawable.shape_bg_download_data);
                setDownloadViewTextColor(R.color.setting_white);
                setDownloadViewIconVisible(true);
                setDownloadViewIcon(R.drawable.img_start_42day);
                break;
            case UserDataCode.TASK_STATUS_CODE_DOING:
            case UserDataCode.TASK_STATUS_CODE_DONE:
                // 下载中 or 更新中
//                String formatted = String.format("%.2f", data.getPercent());// 输出: 123.46
                // Math.floor 四舍五入，向下取整
                setDownloadViewText((int) Math.floor(data.getPercent()) + "%");
                setDownloadViewBackground(R.color.transparent);
                setDownloadViewTextColor(R.color.setting_downloading_color);
                setDownloadViewIconVisible(true);
                setDownloadViewIcon(R.drawable.img_pause);
                break;
            case UserDataCode.TASK_STATUS_CODE_CHECKING:
                // 校验中
                break;
            case UserDataCode.TASK_STATUS_CODE_CHECKED:
                // 校验完成
                break;
            case UserDataCode.TASK_STATUS_CODE_UNZIPPING:
                setDownloadViewText("解压中" + (int) Math.floor(data.getPercent()) + "%");
                setDownloadViewBackground(R.color.transparent);
                setDownloadViewTextColor(R.color.setting_downloading_color);
                setDownloadViewIconVisible(false);
                break;
            case UserDataCode.TASK_STATUS_CODE_UNZIPPED:
                // 解压完成
                break;
            case UserDataCode.TASK_STATUS_CODE_SUCCESS:
                setDownloadViewText("已下载");
                setDownloadViewBackground(0);
                setDownloadViewTextColor(R.color.setting_bg_tab_text_unselect);
                setDownloadViewIconVisible(false);
                mLinearLayout.setGravity(Gravity.CENTER_VERTICAL | Gravity.RIGHT);
                break;
            case UserDataCode.TASK_STATUS_CODE_ERR:
            case UserDataCode.TASK_STATUS_CODE_MAX:
                setDownloadViewText("重试");
                setDownloadViewBackground(R.drawable.shape_bg_download_data);
                setDownloadViewTextColor(R.color.setting_white);
                setDownloadViewIconVisible(false);
                break;
            default:
                break;
        }

    }

}
