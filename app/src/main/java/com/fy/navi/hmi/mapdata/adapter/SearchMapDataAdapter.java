package com.fy.navi.hmi.mapdata.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.scene.ui.setting.DownloadBtnView;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;

import java.util.ArrayList;
import java.util.List;

public class SearchMapDataAdapter extends BaseSearchMapDataAdapter<
        SearchMapDataAdapter.GroupItemViewHolder, SearchMapDataAdapter.SubItemViewHolder> {

    private static final String TAG = SearchMapDataAdapter.class.getSimpleName();
    private Context mContext;
    private OfflineItemListener mOfflineItemListener;
    private List<DataTree<ProvDataInfo, String>> mDts = new ArrayList<>();
    private DownloadBtnView mGroupDownloadBtnView;
    private DownloadBtnView mSubDownloadBtnView;


    public SearchMapDataAdapter(final Context context) {
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

    @SuppressLint("UseCompatLoadingForDrawables")
    @Override
    public void onGroupItemBindViewHolder(final BaseRecyclerHolder holder, final int groupItemIndex) {
        if (mDts != null && !mDts.isEmpty()) {
            final String groupName = mDts.get(groupItemIndex).getGroupItem().getName();
            holder.setText(R.id.item_province_name, groupName); //省份名称
            final int areaType = mDts.get(groupItemIndex).getGroupItem().getAreaType();
            // item 若为城市，则显示城市下载状态；若为省份，则显示展开/关闭按钮
            if (areaType == 2 || areaType == 3) {
                holder.setVisible(R.id.item_download_status, true);
                holder.setVisible(R.id.item_province_expand, false);
                holder.setVisible(R.id.item_province_data, true);
                //城市数据包大小
                final CityDownLoadInfo downloadItem = mDts.get(groupItemIndex).getGroupItem().getDownLoadInfo();
                final String sizeString = StringUtils.formatSize(downloadItem.getFullZipSize());
                holder.setText(R.id.item_province_data, sizeString);
                // 下载按钮状态
                mGroupDownloadBtnView = holder.getView(R.id.item_download_status);
                mGroupDownloadBtnView.parseDownloadStatusInfo(downloadItem);
            } else {
                holder.setVisible(R.id.item_download_status, false);
                holder.setVisible(R.id.item_province_expand, true);
                holder.setVisible(R.id.item_province_data, false);
            }
        }
    }

    @Override
    public void onSubItemBindViewHolder(final BaseRecyclerHolder holder, final int groupItemIndex, final int subItemIndex) {
        if (mDts != null && !mDts.isEmpty()) {
            //城市名称
            final String cityName = mDts.get(groupItemIndex).getSubItems().get(subItemIndex).getName();
            holder.setText(R.id.item_city_name, cityName);
            //城市下载状态
            final CityDownLoadInfo downloadItem = mDts.get(groupItemIndex).getSubItems().get(subItemIndex).getDownLoadInfo();
            //城市数据包大小
            final String sizeString = StringUtils.formatSize(downloadItem.getFullZipSize());
            holder.setText(R.id.item_city_data, sizeString);
            // 下载按钮状态
            mSubDownloadBtnView = holder.getView(R.id.item_download_status);
            mSubDownloadBtnView.parseDownloadStatusInfo(downloadItem);
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
    public void onGroupItemClick(final GroupItemViewHolder holder, final int groupItemIndex) {
        Logger.d(TAG, "group item " + groupItemIndex);
        if (mDts != null && !mDts.isEmpty()) {
            final int taskState = mDts.get(groupItemIndex).getGroupItem().getDownLoadInfo().getTaskState();
            final int adCode = mDts.get(groupItemIndex).getGroupItem().getAdcode();
            final ArrayList<Integer> adCodeList = new ArrayList<>();
            adCodeList.add(adCode);
            if (mOfflineItemListener != null) {
                switch (taskState) {
                    case UserDataCode.TASK_STATUS_CODE_READY: // 待更新 or 待下载
                    case UserDataCode.TASK_STATUS_CODE_PAUSE: // 暂停
                    case UserDataCode.TASK_STATUS_CODE_ERR: // 重试
                    case UserDataCode.TASK_STATUS_CODE_MAX: // 重试
                        mOfflineItemListener.startAllTask(adCodeList);
                        break;
                    case UserDataCode.TASK_STATUS_CODE_WAITING: // 等待中
                    case UserDataCode.TASK_STATUS_CODE_DOING: // 下载中 or 更新中
                    case UserDataCode.TASK_STATUS_CODE_DONE: // 下载中 or 更新中
                        mOfflineItemListener.pauseAllTask(adCodeList);
                        break;
                    default:
                        break;
                }
            }
        }
    }

    @Override
    public void onSubItemClick(final SubItemViewHolder holder, final int groupItemIndex, final int subItemIndex) {
        Logger.d(TAG, "sub item " + subItemIndex + " in group item " + groupItemIndex);

        final CityDataInfo item = mDts.get(groupItemIndex).getSubItems().get(subItemIndex);
        final int adCode = item.getAdcode();
        final ArrayList<Integer> adCodeList = new ArrayList<>();
        adCodeList.add(adCode);

        final CityDownLoadInfo downloadItem = mDts.get(groupItemIndex).getSubItems().get(subItemIndex).getDownLoadInfo();
        if (mOfflineItemListener != null) {
            switch (downloadItem.getTaskState()) {
                case UserDataCode.TASK_STATUS_CODE_READY: // 待更新 or 待下载
                case UserDataCode.TASK_STATUS_CODE_PAUSE: // 暂停
                case UserDataCode.TASK_STATUS_CODE_ERR: // 重试
                case UserDataCode.TASK_STATUS_CODE_MAX: // 重试
                    mOfflineItemListener.startAllTask(adCodeList);
                    break;
                case UserDataCode.TASK_STATUS_CODE_WAITING: // 等待中
                case UserDataCode.TASK_STATUS_CODE_DOING: // 下载中 or 更新中
                case UserDataCode.TASK_STATUS_CODE_DONE: // 下载中 or 更新中
                    mOfflineItemListener.pauseAllTask(adCodeList);
                    break;
                default:
                    break;
            }
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
         * @param adCodeList
         */
        void startAllTask(final ArrayList<Integer> adCodeList);

        /**
         * 暂停下载
         * @param adCodeList
         */
        void pauseAllTask(final ArrayList<Integer> adCodeList);
    }

}
