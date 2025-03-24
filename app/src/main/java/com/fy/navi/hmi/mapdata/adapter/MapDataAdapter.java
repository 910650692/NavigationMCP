package com.fy.navi.hmi.mapdata.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.scene.ui.setting.DownloadBtnView;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;

import java.util.ArrayList;
import java.util.List;

public class MapDataAdapter extends MuliteRecycleAdapter<
        MapDataAdapter.GroupItemViewHolder, MapDataAdapter.SubItemViewHolder> {
    private static final String TAG = MapDataAdapter.class.getSimpleName();
    private Context mContext;
    private OfflineItemListener mOfflineItemListener;
    private List<DataTree<String, String>> mDts = new ArrayList<>();
    private DownloadBtnView mDownloadBtnView;

    public MapDataAdapter(final Context context) {
        this.mContext = context;
    }

    /**
     * 设置数据
     * @param datas
     */
    public void setData(final List datas) {
        mDts = datas;
        notifyNewData(mDts);
    }

    public void setOfflineItemListener(final OfflineItemListener listener) {
        mOfflineItemListener = listener;
    }

    @Override
    public BaseRecyclerHolder groupItemViewHolder(final ViewGroup parent) {
        final View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_province, parent, false);
        return new GroupItemViewHolder(view);
    }

    @Override
    public BaseRecyclerHolder subItemViewHolder(final ViewGroup parent) {
        final View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_city_data, parent, false);
        return new SubItemViewHolder(view);
    }

    @Override
    public void onGroupItemBindViewHolder(final BaseRecyclerHolder holder, final int groupItemIndex) {
        if (mDts != null && !mDts.isEmpty()) {
            final  String groupName = mDts.get(groupItemIndex).getGroupItem();
            holder.setText(R.id.item_province_name, groupName); //省份名称
        }
    }

    @Override
    public void onSubItemBindViewHolder(final BaseRecyclerHolder holder, final int groupItemIndex, final int subItemIndex) {
        if (mDts != null && !mDts.isEmpty()) {
            final CityDownLoadInfo downloadItem = mDts.get(groupItemIndex).getSubItems().get(subItemIndex).getDownLoadInfo();
            //非已下载状态，禁止侧滑删除
            if (downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                holder.setSwipeEnabled(R.id.swipe_menu_layout, true);
            } else {
                holder.setSwipeEnabled(R.id.swipe_menu_layout, false);
            }
            //城市名称
            final String cityName = mDts.get(groupItemIndex).getSubItems().get(subItemIndex).getName();
            holder.setText(R.id.item_city_name, cityName);
            //城市数据包大小
            final String sizeString = StringUtils.formatSize(downloadItem.getFullZipSize());
            holder.setText(R.id.item_city_data, sizeString);
            // 下载按钮状态
            mDownloadBtnView = holder.getView(R.id.item_download_status);
            mDownloadBtnView.parseDownloadStatusInfo(downloadItem);
        }
    }

    @Override
    public void onGroupItemClick(final Boolean isExpand, final GroupItemViewHolder holder, final int groupItemIndex) {
        Logger.d(TAG, "group item " + groupItemIndex + " is expand " + isExpand);
        if (isExpand) {
            holder.setImageResource(R.id.item_province_expand,R.drawable.img_under_the_58);
        } else {
            holder.setImageResource(R.id.item_province_expand, R.drawable.img_up_58);
        }
    }

    @Override
    public void onSubItemClick(final SubItemViewHolder holder, final int groupItemIndex, final int subItemIndex) {
        Logger.d(TAG, "sub item " + subItemIndex + " in group item " + groupItemIndex);

        final CityDownLoadInfo downloadItem = mDts.get(groupItemIndex).getSubItems().get(subItemIndex).getDownLoadInfo();
        final ArrayList<Integer> cityAdcodes = new ArrayList<>();
        cityAdcodes.add(downloadItem.getAdcode());

        if (mOfflineItemListener != null) {
            switch (downloadItem.getTaskState()) {
                case UserDataCode.TASK_STATUS_CODE_DOING:  // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                case UserDataCode.TASK_STATUS_CODE_DONE:   // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                case UserDataCode.TASK_STATUS_CODE_WAITING: // 等待中 or 等待更新中
                    mOfflineItemListener.pauseAllTask(cityAdcodes);
                    break;
                case UserDataCode.TASK_STATUS_CODE_PAUSE:  // 暂停
                case UserDataCode.TASK_STATUS_CODE_READY:  // 待下载 or 待更新 (downloadItem.bIsDataUsed = true 待更新)
                case UserDataCode.TASK_STATUS_CODE_ERR: // 错误 - 重试
                case UserDataCode.TASK_STATUS_CODE_MAX: // 重试
                    mOfflineItemListener.startAllTask(cityAdcodes);
                    break;
                default:
                    break;
            }
        }

    }

    @Override
    public void onSubItemDeleteClick(final SubItemViewHolder holder, final int groupItemIndex, final int subItemIndex) {
        Logger.d(TAG, "sub item " + subItemIndex + " in group item " + groupItemIndex);

        final CityDownLoadInfo downloadItem = mDts.get(groupItemIndex).getSubItems().get(subItemIndex).getDownLoadInfo();
        final ArrayList<Integer> cityAdcodes = new ArrayList<>();
        cityAdcodes.add(downloadItem.getAdcode());

        // 删除已下载 条目
        if (mOfflineItemListener != null) {
            mOfflineItemListener.deleteAllTask(cityAdcodes);
        }
    }

    public static class GroupItemViewHolder extends BaseRecyclerHolder {
        public GroupItemViewHolder(final View itemView) {
            super(itemView);
        }
    }

    public static class SubItemViewHolder extends BaseRecyclerHolder {
        public SubItemViewHolder(final View itemView) {
            super(itemView);
        }
    }

    public interface OfflineItemListener {

        /**
         * 开始下载
         * @param cityAdCodes
         */
        void startAllTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 暂停下载
         * @param cityAdCodes
         */
        void pauseAllTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 删除
         * @param cityAdCodes
         */
        void deleteAllTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 取消下载
         */
        void cancelAllTask();
    }

}
