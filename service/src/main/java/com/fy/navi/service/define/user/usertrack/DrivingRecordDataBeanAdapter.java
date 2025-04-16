package com.fy.navi.service.define.user.usertrack;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import java.io.IOException;

public class DrivingRecordDataBeanAdapter extends TypeAdapter<DrivingRecordDataBean> {
    @Override
    public void write(final JsonWriter out, final DrivingRecordDataBean bean) throws IOException {

    }

    @Override
    public DrivingRecordDataBean read(final JsonReader in) throws IOException {
        final DrivingRecordDataBean bean = new DrivingRecordDataBean();
        in.beginObject();
        while (in.hasNext()) {
            final String fieldName = in.nextName();
            switch (fieldName) {
                case "id":
                    bean.setId(in.nextString());
                    break;
                case "type":
                    bean.setType(in.nextInt());
                    break;
                case "rideRunType":
                    bean.setRideRunType(in.nextInt());
                    break;
                case "timeInterval":
                    bean.setTimeInterval(in.nextInt());
                    break;
                case "startTime":
                    bean.setStartTime(in.nextString());
                    break;
                case "endTime":
                    bean.setEndTime(in.nextString());
                    break;
                case "trackFileName":
                    bean.setTrackFileName(in.nextString());
                    break;
                case "startPoiName":
                    bean.setStartPoiName(in.nextString());
                    break;
                case "endPoiName":
                    bean.setEndPoiName(in.nextString());
                    break;
                case "trackPointsURL":
                    bean.setTrackPointsURL(in.nextString());
                    break;
                case "runDistance":
                    bean.setRunDistance(in.nextInt());
                    break;
                case "maxSpeed":
                    bean.setMaxSpeed((int) Math.round(in.nextDouble()));
                    break;
                case "averageSpeed":
                    bean.setAverageSpeed(in.nextInt());
                    break;
                case "trackFileMd5":
                    bean.setTrackFileMd5(in.nextString());
                    break;
                case "filePath":
                    bean.setFilePath(in.nextString());
                    break;
                default:
                    in.skipValue();
                    break;
            }
        }
        in.endObject();
        return bean;
    }
}