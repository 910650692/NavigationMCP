package com.fy.navi.hmi.mapdata.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;

import java.util.ArrayList;
import java.util.List;

public class MapDataAdapter extends MuliteRecycleAdapter<
        MapDataAdapter.GroupItemViewHolder, MapDataAdapter.SubItemViewHolder> {

    private static final String TAG = MapDataAdapter.class.getSimpleName();
    private Context context;
    private OfflineItemListener offlineItemListener;

    private List<DataTree<String, String>> dts = new ArrayList<>();

    public MapDataAdapter(Context context) {
        this.context = context;
    }

    public void setData(List datas) {
        dts = datas;
        notifyNewData(dts);
    }

    public void setOfflineItemListener(OfflineItemListener listener) {
        offlineItemListener = listener;
    }

    @Override
    public BaseRecyclerHolder groupItemViewHolder(ViewGroup parent) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_province, parent, false);
        return new GroupItemViewHolder(view);
    }

    @Override
    public BaseRecyclerHolder subItemViewHolder(ViewGroup parent) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.item_city_data, parent, false);
        return new SubItemViewHolder(view);
    }

    @Override
    public void onGroupItemBindViewHolder(BaseRecyclerHolder holder, int groupItemIndex) {
        if (dts != null && !dts.isEmpty()) {
            String groupName = dts.get(groupItemIndex).getGroupItem();
            holder.setText(R.id.item_province_name, groupName); //省份名称
        }
    }

    @Override
    public void onSubItemBindViewHolder(BaseRecyclerHolder holder, int groupItemIndex, int subItemIndex) {
        if (dts != null && !dts.isEmpty()) {
            //城市名称
            String cityName = dts.get(groupItemIndex).getSubItems().get(subItemIndex).name;
            holder.setText(R.id.item_city_name, cityName);
            //城市下载状态
            CityDownLoadInfo downloadItem = dts.get(groupItemIndex).getSubItems().get(subItemIndex).downLoadInfo;
            if (downloadItem.statusTip.equals("下载中")) {
                holder.setText(R.id.item_status_tip, downloadItem.statusTip + downloadItem.percent);
            } else {
                holder.setText(R.id.item_status_tip, downloadItem.statusTip);
            }

            if (downloadItem.taskState == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                holder.setVisible(R.id.item_btn_status, false);
            } else {
                holder.setVisible(R.id.item_btn_status, true);
            }

            //城市数据包大小
            String sizeString = StringUtils.formatSize(downloadItem.nFullZipSize);
            holder.setText(R.id.item_city_data, sizeString);
        }
    }

    @Override
    public void onGroupItemClick(Boolean isExpand, GroupItemViewHolder holder, int groupItemIndex) {
        Logger.d(TAG, "group item " + groupItemIndex + " is expand " + isExpand);
        if (isExpand) {
            holder.setImageResource(R.id.item_province_expand,R.drawable.img_under_the_58);
        } else {
            holder.setImageResource(R.id.item_province_expand, R.drawable.img_up_58);
        }
    }

    @Override
    public void onSubItemClick(SubItemViewHolder holder, int groupItemIndex, int subItemIndex) {
        Logger.d(TAG, "sub item " + subItemIndex + " in group item " + groupItemIndex);

        CityDownLoadInfo downloadItem = dts.get(groupItemIndex).getSubItems().get(subItemIndex).downLoadInfo;

        ArrayList<Integer> cityAdcodes = new ArrayList<>();
        cityAdcodes.add(downloadItem.adcode);

        if (offlineItemListener != null) {
            switch (downloadItem.taskState) {
                case UserDataCode.TASK_STATUS_CODE_DOING:  // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                case UserDataCode.TASK_STATUS_CODE_DONE:   // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                case UserDataCode.TASK_STATUS_CODE_WAITING: // 等待中 or 等待更新中
                    offlineItemListener.pauseAllTask(cityAdcodes);
                    break;
                case UserDataCode.TASK_STATUS_CODE_PAUSE:  // 暂停
                case UserDataCode.TASK_STATUS_CODE_READY:  // 待下载 or 待更新 (downloadItem.bIsDataUsed = true 待更新)
                case UserDataCode.TASK_STATUS_CODE_ERR: // 错误 - 重试
                case UserDataCode.TASK_STATUS_CODE_MAX: // 重试
                   /* long size = downloadItem.nFullZipSize;.getTotalSize();
                    if(downloadItem.bIsDataUsed) { // 待更新
                        if (downloadItem.IsCompltelyHighVer) {
                            // 全量更新，数据包待更新的大小用CityDownLoadItem.nFullZipSize字段值来显示
                        } else {
                            // 增量更新，数据包待更新的大小用CityDownLoadItem.nZipSize字段值来显示
                        }
                    } else {   // 待下载
                        // 待下载，数据包待下载的大小用CityDownLoadItem.nFullZipSize字段值来显示
                    }*/
                    offlineItemListener.startAllTask(cityAdcodes);
                    break;

               /* case DownloadStatus.ERROR:
                case DownloadStatus.ERROR_UPDATE:
                    long size = item.getTotalSize();
                    if (item.getStatus() == DownloadStatus.NEED_UPDATE
                            || item.getStatus() == DownloadStatus.ERROR_UPDATE) {
                        size = item.getUpdateSize();
                    } else if (item.getStatus() == DownloadStatus.SUSPEND) {
                        if (item.getUpdateSize() != 0) {
                            size = item.getUpdateSize() - item.getDownloadUpdateSize();
                        } else {
                            size = item.getTotalSize() - item.getDownloadSize();
                        }
                    }
                    offlineItemListener.download(item.getId(), size);
                    break;*/

                default:
                    break;
            }
        }

    }

    public static class GroupItemViewHolder extends BaseRecyclerHolder {
        public GroupItemViewHolder(View itemView) {
            super(itemView);
        }
    }

    public static class SubItemViewHolder extends BaseRecyclerHolder {
        public SubItemViewHolder(View itemView) {
            super(itemView);
        }
    }

    public interface OfflineItemListener {

        void startAllTask(ArrayList<Integer> cityAdCodes);

        void pauseAllTask(ArrayList<Integer> cityAdCodes);

        void cancelAllTask();
    }

}
